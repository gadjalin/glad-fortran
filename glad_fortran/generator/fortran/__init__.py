from collections import defaultdict
import re
import jinja2

import glad
from glad.config import Config, ConfigOption
from glad.generator import JinjaGenerator
from glad.generator.util import (
    strip_specification_prefix,
    collect_alias_information,
    find_extensions_with_aliases,
    jinja2_contextfilter
)
from glad.parse import ParsedType, EnumType
from glad.sink import LoggingSink

_F_TO_C_TYPE = {
    'char': 'c_char',
    'uchar': 'c_char',
    'float': 'c_float',
    'double': 'c_double',
    'int': 'c_int',
    'long': 'c_long',
    'int8_t': 'c_int8_t',
    'uint8_t': 'c_int8_t',
    'int16_t': 'c_int16_t',
    'uint16_t': 'c_int16_t',
    'int32_t': 'c_int32_t',
    'uint32_t': 'c_int32_t',
    'int64_t': 'c_int64_t',
    'uint64_t': 'c_int64_t',
    'size_t': 'c_size_t',
    'ull': 'c_int64_t',
}

_GL_INT_TYPES = [
    'GLbyte', 'GLubyte',
    'GLshort', 'GLushort',
    'GLint', 'GLuint', 'GLint64', 'GLuint64', 'GLint64EXT', 'GLuint64EXT',
    'GLintptr', 'GLsizeiptr', 'GLintptrARB', 'GLsizeiptrARB',
    'GLsizei', 'GLclampx', 'GLfixed', 'GLhalf', 'GLhalfNV', 'GLhalfARB',
    'GLenum', 'GLbitfield',
    'GLvdpauSurfaceNV'
]

_GL_REAL_TYPES = [
    'GLfloat', 'GLdouble',
    'GLclampf', 'GLclampd'
]

_GL_CHAR_TYPES = [
    'GLchar', 'GLcharARB'
]

_GL_TYPEDEF_PTR = [
    'GLsync', 'GLeglClientBufferEXT', 'GLeglImageOES'
]

_CL_TYPES = [
    '_cl_context', '_cl_event'
]

_GL_TYPEDEF_FUNPTR = [
    'GLDEBUGPROC', 'GLDEBUGPROCARB', 'GLDEBUGPROCKHR', 'GLDEBUGPROCAMD',
    'GLVULKANPROCNV'
]

_GL_TYPE_SUFFIXES = [
    'ui64v', 'ui64',
    'ubv', 'usv', 'uiv',
    'bv', 'sv', 'iv', 'fv', 'dv',
    'ub', 'us', 'ui',
    'v', 'b', 's', 'i', 'f', 'd'
]

_GL_VENDOR_SUFFIXES = [
    'NV', 'AMD', 'APPLE', 'ARB', 'ATI', 'EXT', 'INTEL', 'KHR', 'MESA', 'OES', 'SUN'
]

_F_INT_KINDS = [
    'c_short', 'c_int', 'c_long', 'c_long_long',
    'c_signed_char', 'c_size_t', 'c_intptr_t',
    'c_int8_t', 'c_int16_t', 'c_int32_t', 'c_int64_t'
]

_F_REAL_KINDS = [
    'c_float', 'c_double', 'c_long_double'
]

_F_CHAR_KINDS = [
    'c_char'
]


# --- Generate GL enumerated values
def enum_type_kind(enum, feature_set):
    # Hex constant
    if enum.value.startswith('0x'):
        return 'c_int64_t' if len(enum.value[2:]) > 8 else 'c_int'
    # GL bool constant
    elif enum.name in ['GL_TRUE', 'GL_FALSE']:
        return 'c_signed_char'
    # Integer number
    elif enum.value.lstrip('+-').isdigit():
        return 'c_int'
    else:
        return NotImplementedError(f'Unhandled enumeration type kind: {enum.name} = {enum.value}')


def enum_type(enum, feature_set):
    kind = enum_type_kind(enum, feature_set)

    if kind in _F_INT_KINDS:
        return 'integer(kind={})'.format(kind)
    elif kind in _F_REAL_KINDS:
        return 'real(kind={})'.format(kind)
    elif kind in _F_CHAR_KINDS:
        return 'character(len=*,kind={})'.format(kind)
    else:
        raise RuntimeError(f'Unknown enumeration type for kind: {kind}')


def enum_value(enum, feature_set):
    kind = enum_type_kind(enum, feature_set)
    value = enum.value

    if kind in _F_INT_KINDS:
        if value.startswith('0x'):
            boz_size = 16 if len(value[2:]) > 8 else 8 # number of hex digits for int64 or int32
            boz_value = value[2:].zfill(boz_size)
            return f'int(Z\'{boz_value}\', kind={kind})'
        else:
            # Remove decorative characters
            # TODO bitwise not (~)
            return re.sub(r'[()UL]', '', value)
    else:
        raise RuntimeError(f'Unknown enumeration value for kind: {kind} and {value}')


# --- Generate Fortran function/subroutine interfaces to
#     C functions using appropriate type kinds for C function arguments
def interface_return_type(command):
    type_ = command.proto.ret

    parsed_type = get_parsed_type(type_)

    # Sanity check
    if (parsed_type.is_pointer > 2) or \
       (is_void(parsed_type) and not parsed_type.is_pointer):
        raise NotImplementedError(f'Unhandled or void return type')

    # This should be straightforward for C interfaces
    if parsed_type.is_pointer > 0 or \
       is_typedef_ptr(parsed_type):
        return 'type(c_ptr)'
    elif is_typedef_funptr(parsed_type):
        return 'type(c_funptr)'
    elif is_int(parsed_type) or is_bool(parsed_type):
        return f'integer(kind={parsed_type.type})'
    elif is_real(parsed_type):
        return f'real(kind={parsed_type.type})'
    elif is_GLhandleARB(parsed_type):
        return macro_type(parsed_type.type)
    else:
        raise NotImplementedError(f'Unhandled return type: {parsed_type.type}')


def interface_param_type(type_):
    parsed_type = get_parsed_type(type_)

    # Sanity check
    if (parsed_type.is_pointer > 2) or \
       (is_void(parsed_type) and not parsed_type.is_pointer):
        raise NotImplementedError(f'Unhandled parameter type: {parsed_type.type}{'*'*parsed_type.is_pointer}')

    type_decl = ''

    # Base type
    # This should be straightforward for C interfaces
    # Non const char* should be buffers so we treat them as pointers instead of
    # converting to null-terminated C string
    if is_char(parsed_type) and parsed_type.is_pointer == 1 and parsed_type.is_const:
        type_decl = f'character(len=1,kind={parsed_type.type})'
    elif is_char(parsed_type) and parsed_type.is_pointer == 2 and parsed_type.is_const:
        type_decl = f'type(c_ptr)'
    # Because pointers to scalars and arrays are ambiguous -> all c_ptr
    elif is_typedef_ptr(parsed_type) or is_cl_ptr(parsed_type):
        type_decl = 'type(c_ptr)'
    elif is_typedef_funptr(parsed_type):
        type_decl = 'type(c_funptr)'
    elif is_GLhandleARB(parsed_type):
        type_decl = macro_type(parsed_type.type)
    elif parsed_type.is_pointer > 0:
        type_decl = 'type(c_ptr)'
    # Then handle non-pointer
    elif is_int(parsed_type) or is_bool(parsed_type):
        type_decl = f'integer(kind={parsed_type.type})'
    elif is_real(parsed_type):
        type_decl = f'real(kind={parsed_type.type})'
    else:
        raise NotImplementedError(f'Unhandled parameter type: {parsed_type.type}{'*'*parsed_type.is_pointer}')

    # Additional type specs
    if is_char(parsed_type) and parsed_type.is_const:
        type_decl += ', dimension(*)'

    if parsed_type.is_pointer == 0 or \
       (parsed_type.is_pointer == 1 and
        not (is_char(parsed_type) and parsed_type.is_const)) or \
       is_GLhandleARB(parsed_type):
        type_decl += ', value'

    if parsed_type.is_const or \
       (parsed_type.is_pointer == 0 and (is_int(parsed_type) or is_bool(parsed_type) or is_real(parsed_type))):
        type_decl = type_decl + ', intent(in)'

    return type_decl


# --- Fortran implementations of GL functions wrap calls to
#     generated C function interfaces
def impl_return_type(command):
    type_ = command.proto.ret

    parsed_type = get_parsed_type(type_)

    # Actually return strings but type would give "const GLubyte*"
    # It should in principle return a pointer to a static string,
    # but this makes the fortran syntax ugly, so it's copied and is probably
    # not performance critical or anything
    # if parsed_type.type == 'GLubyte' and parsed_type.is_const and parsed_type.is_pointer == 1:
    if command.name in ['glGetString', 'glGetStringi']:
        return 'character(len=:, kind=c_char), pointer'
    elif (parsed_type.is_pointer == 1 and is_void(parsed_type)) or \
         is_typedef_ptr(parsed_type):
        return 'type(c_ptr)'
    # I think only glGetVkProcAddrNV does this
    elif is_typedef_funptr(parsed_type):
        return f'procedure({parsed_type.type}), pointer'
    elif is_GLhandleARB(parsed_type):
        return macro_type(parsed_type.type)
    # Sanity check
    elif parsed_type.is_pointer > 0:
        raise NotImplementedError(f'Unhandled pointer return type: {parsed_type.type}{'*'*parsed_type.is_pointer}')
    elif is_int(parsed_type) or is_bool(parsed_type):
        return f'integer(kind={parsed_type.type})'
    elif is_real(parsed_type):
        return f'real(kind={parsed_type.type})'
    else:
        raise NotImplementedError(f'Unhandled return type: {parsed_type.type}')


def impl_param_type(type_, command):
    parsed_type = get_parsed_type(type_)

    type_decl = ''

    if is_char(parsed_type) and parsed_type.is_pointer == 1:
        type_decl = f'character(len=*,kind={parsed_type.type})'
    elif is_char(parsed_type) and parsed_type.is_pointer == 2 and parsed_type.is_const:
        type_decl = f'character(len=:,kind={parsed_type.type})'
    # TODO Compatibility, Assumed type is Fortran 2018
    elif is_void(parsed_type) and parsed_type.is_pointer == 1:
        type_decl = 'type(*)'
    elif is_typedef_ptr(parsed_type) or is_cl_ptr(parsed_type):
        type_decl = 'type(c_ptr)'
    elif parsed_type.is_pointer == 2:
        type_decl = 'type(c_ptr)'
    elif is_typedef_funptr(parsed_type):
        if command.name == 'glDebugMessageCallback':
            return 'procedure(GLDEBUGPROC), optional'
        elif command.name == 'glDebugMessageCallbackAMD':
            return 'procedure(GLDEBUGPROCAMD), optional'
        elif command.name == 'glDebugMessageCallbackARB':
            return 'procedure(GLDEBUGPROCARB), optional'
    elif is_GLhandleARB(parsed_type):
        type_decl = macro_type(parsed_type.type)
    elif is_int(parsed_type) or is_bool(parsed_type):
        type_decl = f'integer(kind={parsed_type.type})'
    elif is_real(parsed_type):
        type_decl = f'real(kind={parsed_type.type})'
    else:
        raise NotImplementedError(f'Unhandled parameter type: {parsed_type.type}{'*'*parsed_type.is_pointer}')

    if is_char(parsed_type) and parsed_type.is_pointer == 2 and parsed_type.is_const:
        type_decl += ', dimension(:)'
        type_decl += ', pointer'

    if parsed_type.is_pointer == 1 and not (is_char(parsed_type) and parsed_type.is_const):
        type_decl += ', target'

    if is_optional_type(parsed_type):
        type_decl += ', optional'

    if parsed_type.is_const:
        type_decl += ', intent(in)'

    return type_decl


# --- Intermediate variables in the implementations are used to wrap calls to c_loc/c_funloc (for
#     pointers) and handle Fortran to C string conversions
def is_requiring_int_var(param):
    type_ = param.type
    parsed_type = get_parsed_type(type_)

    return is_char(parsed_type) and parsed_type.is_pointer == 2

def is_requiring_int_result(command):
    ret_type = command.proto.ret
    parsed_type = get_parsed_type(ret_type)

    return command.name in ('glGetString', 'glGetStringi')


def intermediate_var(param):
    type_ = param.type
    name = param.name
    parsed_type = get_parsed_type(type_)

    if is_char(parsed_type) and parsed_type.is_pointer == 2:
        return f'character(len=:,kind={parsed_type.type}), dimension(:), allocatable :: {int_identifier(name)}str\n' + \
               ' '*12 + f'type(c_ptr), dimension(:), allocatable :: {int_identifier(name)}'
    else:
        raise NotImplementedError(f'Unhandled intermediate type: {parsed_type.type}{'*'*parsed_type.is_pointer}')


def int_var_pass(param):
    type_ = param.type
    name = param.name
    parsed_type = get_parsed_type(type_)

    if is_char(parsed_type) and parsed_type.is_pointer == 2:
        return f'call f_c_strarray({arg_identifier(name)}, {int_identifier(name)}str, {int_identifier(name)})'
    else:
        raise NotImplementedError(f'Unhandled intermediate type: {parsed_type.type}{'*'*parsed_type.is_pointer}')


# --- C Pointer arguments shown as Fortran optional arguments using intermediate
#     type(c_ptr) variable to give a sense of passing null pointers
def is_optional(param):
    type_ = param.type
    parsed_type = get_parsed_type(type_)

    return is_optional_type(parsed_type)


def is_optional_type(parsed_type):
    return (parsed_type.is_pointer > 0 or is_typedef_ptr(parsed_type) or is_cl_ptr(parsed_type) or is_typedef_funptr(parsed_type)) and \
           not is_char(parsed_type) and not is_GLhandleARB(parsed_type)


def opt_type(type_):
    parsed_type = get_parsed_type(type_)

    if parsed_type.is_pointer > 0 or \
         is_typedef_ptr(parsed_type) or is_cl_ptr(parsed_type):
        return 'type(c_ptr)'
    elif is_typedef_funptr(parsed_type):
        return 'type(c_funptr)'
    else:
        raise RuntimeError(f'Unhandled optional type: {parsed_type.type}{'*'*parsed_type.is_pointer}')


def opt_identifier(name):
    return 'opt_' + arg_identifier(name)


def opt_pass(param):
    type_ = param.type
    name = param.name
    parsed_type = get_parsed_type(type_)

    if parsed_type.is_pointer == 1 and \
       (is_void(parsed_type) or is_int(parsed_type) or is_bool(parsed_type) or is_real(parsed_type)):
        return f'{opt_identifier(name)} = c_loc({arg_identifier(name)})'
    elif is_typedef_funptr(parsed_type):
        return f'{opt_identifier(name)} = c_funloc({arg_identifier(name)})'
    else:
        return f'{opt_identifier(name)} = {arg_identifier(name)}'


def opt_null(param):
    type_ = param.type
    name = param.name
    parsed_type = get_parsed_type(type_)

    if is_typedef_funptr(parsed_type):
        return f'{opt_identifier(name)} = c_null_funptr'
    else:
        return f'{opt_identifier(name)} = c_null_ptr'


# --- Generate function signature/call argument list
def input_arg_list(command):
    # Compilers will usually complain about lines longer than like 132 lines
    # in fortran so add a lot of line breaks
    if len(command.params) > 0:
        return ' &\n                ' + \
               ', &\n                '.join(arg_identifier(param.name) for param in command.params) + \
               ' &\n        '
    else:
        return ''


def forward_arg(param):
    type_ = param.type
    name = param.name
    parsed_type = get_parsed_type(type_)

    if is_char(parsed_type) and parsed_type.is_pointer == 1:
        if parsed_type.is_const or name == 'glGetPerfQueryIdByNameINTEL':
            return f'f_c_str({arg_identifier(name)})'
        else:
            return f'c_loc({arg_identifier(name)})'
    elif is_optional(param):
        return opt_identifier(name)
    elif is_requiring_int_var(param):
        return int_identifier(name)
    else:
        return arg_identifier(name)


def int_arg_list(command):
    if len(command.params) > 0:
        return ' &\n                ' + \
               ', &\n                '.join(forward_arg(param) for param in command.params) + \
               ' &\n            '
    else:
        return ''


def format_result(command):
    ret_type = command.proto.ret
    parsed_type = get_parsed_type(ret_type)

    if command.name in ('glGetString', 'glGetStringi'):
        return 'call c_f_strptr(int_res, res, c_strlen(int_res))'
    elif is_typedef_funptr(parsed_type):
        return 'call c_f_procpointer(' + proc_pointer(command.name) + \
               '(' + int_arg_list(command) + ')' + ', res)'
    else:
        return 'res = ' + proc_pointer(command.name) + '(' + int_arg_list(command) + ')'


# --- Generate generic interfaces for some GL function variants
#def split_function_name(func):
#    basename = func
#    vendor_id = ''
#    for v in _GL_VENDOR_SUFFIXES:
#        if basename.endswith(v):
#            vendor_id = v
#            basename = basename[:-len(v)]
#            break
#
#    type_suffix = ''
#    for t in _GL_TYPE_SUFFIXES:
#        # Try to avoid stipping the 'd' at the end of e.g. 'Indexed' or 'Enabled'
#        if basename.endswith(t) and \
#           not ((t == 'd' and basename.endswith('ed')) or \
#                (t == 'dv' and basename.endswith('edv'))):
#            basename = basename[:-len(t)]
#            type_suffix = t
#            break
#
#    return basename, type_suffix, vendor_id
#
#def group_functions(func_list):
#    groups = defaultdict(list)
#    for func in func_list:
#        basename, type_suffix, vendor_id = split_function_name(func)
#        key = basename + vendor_id
#        groups[key].append(func)
#    return {k: v for k,v in groups.items() if len(v) > 1}
#
#
#def make_generic_interfaces(commands):
#    names = [command.name for command in commands]
#    groups = group_functions(names)
#    block = ''
#    for name,variants in groups.items():
#        block += '\n    interface ' + name
#        # Again we must care about line truncation
#        for variant in variants:
#            block += '\n        module procedure ' + variant
#        block += '\n    end interface ' + name
#
#    return block


# ---  Type testing functions
def is_returning(command):
    ret_type = command.proto.ret
    parsed_type = get_parsed_type(ret_type)

    # If prototype return type is void (but not void*), it is not returning
    return not (not parsed_type.is_pointer and parsed_type.type == 'void')


def is_void(parsed_type):
    return parsed_type.type == 'void' or parsed_type.type == 'GLvoid'


def is_int(parsed_type):
    return parsed_type.type in _GL_INT_TYPES


def is_bool(parsed_type):
    return parsed_type.type == 'GLboolean'


def is_real(parsed_type):
    return parsed_type.type in _GL_REAL_TYPES


def is_char(parsed_type):
    return parsed_type.type in _GL_CHAR_TYPES


def is_typedef_ptr(parsed_type):
    return parsed_type.type in _GL_TYPEDEF_PTR


def is_cl_ptr(parsed_type):
    # _cl_context and _cl_event types can have
    # leading blank spaces for some reason
    return parsed_type.type.strip() in _CL_TYPES


def is_typedef_funptr(parsed_type):
    return parsed_type.type in _GL_TYPEDEF_FUNPTR


# This type must be handled seperately because of different definitions on mac
def is_GLhandleARB(parsed_type):
    return parsed_type.type == 'GLhandleARB'


# --- Naming functions
def arg_identifier(name):
    return name


def int_identifier(name):
    return 'int_' + arg_identifier(name)


def interface_proc_name(name):
    return 'c_' + name + 'Proc'


def impl_proc_name(name):
    return name


def proc_pointer(name):
    return 'glad_' + name


def macro_type(type_name):
    return 'type(' + type_name + ')'


# --- Utility functions
def get_parsed_type(type_):
    if isinstance(type_, ParsedType):
        return type_
    elif isinstance(type_, str):
        return ParsedType.from_string(type_)
    else:
        raise RuntimeError(f'Unknown return type: {type_}')




#import re
#from collections import defaultdict
#
## Regex for OpenGL function names
## Captures: base (glSomething), type suffix (i, f, d, iv, fv, etc.), extension suffix (ARB, AMD...)
#func_pattern = re.compile(r'^(gl[^(0-9]*[0-9]?[0-9]?)'  # base, e.g. glVertex3
#                          r'([a-z]+)?'               # type suffix (i, f, iv, fv...)
#                          r'([A-Z]{2,})?$')             # extension suffix (ARB, AMD...)
#
#def group_opengl_functions(func_names):
#    groups = defaultdict(list)
#    for name in func_names:
#        match = func_pattern.match(name)
#        if not match:
#            continue
#        base, type_suffix, ext_suffix = match.groups()
#        type_suffix = type_suffix or ''
#        ext_suffix = ext_suffix or ''
#        # Normalised key = base without type or extension suffix
#        key = base
#        groups[key].append({
#            "full": name,
#            "type_suffix": type_suffix,
#            "extension": ext_suffix
#        })
#    return groups
#
#


class FortranConfig(Config):
    pass
    # TODO 
    #ALIAS = ConfigOption(
    #    converter=bool,
    #    default=False,
    #    description='Automatically adds all extensions that ' +
    #                'provide aliases for the current feature set.'
    #)
    # MX = ConfigOption(
    #     converter=bool,
    #     default=False,
    #     description='Enables support for multiple GL contexts'
    # )


class FortranGenerator(JinjaGenerator):
    DISPLAY_NAME = 'Fortran'

    TEMPLATES = ['glad_fortran.generator.fortran']
    Config = FortranConfig

    def __init__(self, *args, **kwargs):
        JinjaGenerator.__init__(self, *args, **kwargs)

        self.environment.filters.update(
            enum_type=jinja2_contextfilter(lambda ctx, enum: enum_type(enum, ctx['feature_set'])),
            enum_value=jinja2_contextfilter(lambda ctx, enum: enum_value(enum, ctx['feature_set'])),

            interface_proc_name=interface_proc_name,
            interface_param_type=interface_param_type,
            interface_return_type=interface_return_type,
            impl_proc_name=impl_proc_name,
            impl_param_type=impl_param_type,
            impl_return_type=impl_return_type,

            format_result=format_result,
            intermediate_var=intermediate_var,
            input_arg_list=input_arg_list,
            int_arg_list=int_arg_list,
            identifier=arg_identifier,
            int_identifier=int_identifier,
            opt_identifier=opt_identifier,
            int_var_pass=int_var_pass,
            opt_type=opt_type,
            opt_pass=opt_pass,
            opt_null=opt_null,
            proc_pointer=proc_pointer,

#            make_generic_interfaces=make_generic_interfaces,
        )

        self.environment.tests.update(
            returning=is_returning,
            requiring_int_var=is_requiring_int_var,
            requiring_int_result=is_requiring_int_result,
            optional=is_optional
        )

    @property
    def id(self):
        return 'fortran'

    def select(self, spec, api, version, profile, extensions, config, sink=LoggingSink(__name__)):
        if extensions is not None:
            extensions = set(extensions)

        # TODO
        #    if config['ALIAS']:
        #        extensions.update(find_extensions_with_aliases(spec, api, version, profile, extensions))

        return JinjaGenerator.select(self, spec, api, version, profile, extensions, config, sink=sink)

    def get_template_arguments(self, spec, feature_set, config):
        args = JinjaGenerator.get_template_arguments(self, spec, feature_set, config)

        args.update(
            version=glad.__version__,
            aliases=collect_alias_information(feature_set.commands)
        )

        return args

    def get_templates(self, spec, feature_set, config):
        return [
            ('base_template.F90', 'src/{}.F90'.format(feature_set.name, spec.name)),
            ('types.h', 'include/glad/types.h'.format(feature_set.name))
        ]

    def modify_feature_set(self, spec, feature_set, config):
        self._remove_empty_enums(feature_set)

        return feature_set

    def _remove_empty_enums(self, feature_set):
        """
        There are some enums which are simply empty:
        https://github.com/KhronosGroup/Vulkan-Docs/issues/1754
        they need to be removed, we need to also remove any type which is an alias to that empty enum.
        """
        to_remove = set()

        for typ in (t for t in feature_set.types if isinstance(t, EnumType)):
            if typ.alias is None and not typ.enums_for(feature_set):
                to_remove.add(typ.name)

        feature_set.types = [t for t in feature_set.types if t.name not in to_remove and t.alias not in to_remove]

