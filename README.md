# glad-fortran

&nbsp;&nbsp; *"I think you underestimated the problem..."*

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; *"That's what I do... And then I figure out."*

---

## Description

Fortran generator extension for [glad2](https://github.com/Dav1dde/glad).

Glad2 fetches the latest official specification of OpenGL (and many of its
extensions) and uses python to generate interfaces.

This extension is the only available tool capable of producing complete, modern Fortran interfaces for the
latest version of OpenGL and its extensions (as far as I know).

Support for other specifications than OpenGL (EGL, GLES, etc) may be done on request.
The generated Fortran code requires at least Fortran 2018 and a preprocessor to compile.

## Examples

```fortran
program gl_glfw
    ! Fortran bindings for GLFW: https://github.com/AarnoldGad/glf90w
    use glf90w
    ! OpenGL core profile generated using: glad --api gl:core=3.3 --out-path . fortran
    use gl

! Required for a few platform-dependent type definitions
#include "glad/types.h"

    implicit none
    ! ...

    window = glfwCreateWindow(800, 600, 'GLF90W Basic App')
    call glfwMakeContextCurrent(window)

    if (.not. gladLoadGL(glfwGetProcAddress)) stop 'Failed to initialise OpenGL context!'

    ! ...

end program gl_glfw
```

Full example can be found in [gl\_glfw.F90](./examples/gl_glfw.F90).

## Notes

### General considerations

One complication when interfacing C code with Fortran is pointers. Fortran
pointers work very differently from C, and OpenGL uses many pointers for
different purposes. These pointers can be used as input for scalars or arrays,
and of unknown types (i.e. as `void*`), or to instead return data to the caller.
All of these cases are well defined in Fortran and can cause interfaces to look quite different,
but the intent of the C functions is not always clear.

The goal of this module is to provide "Fortran-ic" interfaces to OpenGL as much
as possible. Thus, I want to keep the user from having to mess with the
`iso_c_binding` module as much as possible, but defining clear use cases for
each OpenGL function and have interfaces handle all the corner cases properly
would take too much time and effort, so I instead rely on simple rules and some concessions.

### Passing arrays

For this reason, one important thing to remember when passing arrays to
functions is to always pass the first element instead, and make sure that the
data is stored contiguously in said array

```fortran
real, dimension(3) :: v
! ...

call glVertex3fv(v) ! Wrong
call glVertex3dv(v(1)) ! Right
```

In this example, the C function expects a pointer (i.e. to the first element) but the Fortran
interface is generated to take a scalar as input. This is because some other function
may be able to handle both and generating code on a case-by-case basis is complicated.
Therefore, the Fortran code takes scalars, which is more generic in the limit
that one has to pass arrays in this manner.

### Testing

As said, there are many functions in the OpenGL specification (especially
including all the extensions), and each may use or interpret parameters in a
different way. This generator is made to be very generic and has not been properly
tested yet. The generated C interfaces are expected to work no matter what because OpenGL functions only
use simple types in their signatures. However, the Fortran wrappers
may produce unexpected results (in particular when the C function is taking pointers) if
some corner case has not been handled or predicted. That is, until it has been
tested at least once, which I have not done for most of these functions.

So please, if you find something unepected when calling some OpenGL function
(again, be on the lookout for those taking pointers) from Fortran, make sure to
test it in C or by calling the C interface directly from the Fortran module.
And then open an issue on the github repository.

## Credits

- [Gaétan J.A.M. Jalin](https://github.com/AarnoldGad): [glad-fortran](https://github.com/AarnoldGad/glad-fortran)
- [David Herberth](https://github.com/Dav1dde): [glad](https://github.com/Dav1dde/glad)

## License

See [LICENSE](/LICENSE).
cf. [glad](https://github.com/Dav1dde/glad) for details on the license of the generated code.

