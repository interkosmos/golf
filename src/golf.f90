! golf.f90
!
! Conway's Game of Life, written in Fortran 2003 for Unix/Linux.
! The world can be loaded from a text file, for instance:
!
! $ ./golf --file world.txt --columns 30 --rows 10
!
! Compilation:
!
! $ mkdir build && cd build/
! $ cmake ..
! $ make
!
! Command-line Arguments:
!
!   -v, --version
!       Game version.

!   -g, --generations
!       Number of generations to run (default: 60).

!   -f, --file
!       File name of world to load.

!   -c, --columns
!       Columns of the world (default: 30).

!   -r, --rows
!       Rows of the world (default: 10).
!
! Author:  Philipp Engel
! Licence: ISC
module handlers
    implicit none
contains
    subroutine sigint_handler()
        print '(2a)', achar(27), '[?25h'
        stop
    end subroutine sigint_handler
end module handlers

program life
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit
    use, intrinsic :: iso_c_binding, only: c_int, c_int32_t
    use :: handlers
    implicit none

    character(len=*), parameter :: ANSI_ESC   = achar(27)
    integer,          parameter :: SLEEP_TIME = 250 * 1000
    real,             parameter :: VERSION    = 0.5

    logical, allocatable :: world(:, :)     ! World map.
    logical, allocatable :: buffer(:, :)    ! Buffered world map.
    character(len=256)   :: file_name = ''  ! File name of the stored world.
    integer              :: width     = 30  ! Num. of columns. (default: 30)
    integer              :: height    = 10  ! Num. of rows. (default: 10)
    integer              :: max_gen   = 60  ! Maximum number of generations (default: 60).
    integer              :: gen             ! Current generation.
    integer              :: rc

    interface
        function c_usleep(useconds) bind(c, name='usleep')
            !! Interface to `usleep()` in libc.
            import :: c_int, c_int32_t
            integer(c_int32_t), value :: useconds
            integer(c_int)            :: c_usleep
        end function c_usleep
    end interface

    ! Catch SIGINT.
    call signal(2, sigint_handler)

    call hide_cursor()
    call cls()

    ! Init everything.
    call read_arguments(version, file_name, width, height, max_gen)

    allocate ( world(width, height))
    allocate (buffer(width, height))

    call read_map(file_name, world, width, height)

    ! Main loop.
    do gen = 1, max_gen
        call reset_cursor()
        print '(a, /)', "CONWAY'S GAME OF LIFE"
        call output(world, width, height)
        print '(/, a, i0, a, i0)', 'Generation: ', gen, '/', max_gen

        call next(world, buffer, width, height)
        rc = c_usleep(SLEEP_TIME)
    end do

    ! Quit.
    if (allocated(world))  deallocate (world)
    if (allocated(buffer)) deallocate (buffer)

    call show_cursor()
contains
    subroutine read_arguments(version, file_name, width, height, max_gen)
        !! Reads command-line arguments.
        use :: f90getopt
        real,               intent(in)  :: version
        character(len=256), intent(out) :: file_name
        integer,            intent(out) :: width
        integer,            intent(out) :: height
        integer,            intent(out) :: max_gen
        type(option_type)               :: opts(5)

        opts(1) = option_type('rows',        .true.,  'r')
        opts(2) = option_type('columns',     .true.,  'c')
        opts(3) = option_type('file',        .true.,  'f')
        opts(4) = option_type('generations', .true.,  'g')
        opts(5) = option_type('version',     .false., 'v')

        do
            select case (getopt('l:c:f:g:v', opts))
                case (char(0))
                    exit
                case ('l')
                    read (opt_arg, '(i3)') height
                case ('c')
                    read (opt_arg, '(i3)') width
                case ('f')
                    read (opt_arg, '(a)') file_name
                case ('g')
                    read (opt_arg, '(i3)') max_gen
                case ('v')
                    print '(a, f3.1)', 'Game of Life ', version
                    stop
            end select
        end do
    end subroutine read_arguments

    subroutine read_map(file_name, world, width, height)
        !! Loads world from text file.
        character(len=256),   intent(in)    :: file_name
        logical, allocatable, intent(inout) :: world(:, :)
        integer,              intent(in)    :: width
        integer,              intent(in)    :: height
        character(len=width)                :: line
        integer                             :: fu, rc, x, y

        if (len_trim(file_name) == 0) then
            write (stderr, '(a)') 'Invalid file name'
            stop
        end if

        open (newunit=fu, file=trim(file_name), action='read', iostat=rc)

        if (rc /= 0) then
            write (stderr, '(3a, i0)') 'Reading file "', trim(file_name), '" failed: error ', rc
            stop
        else
            do y = 1, height
                read (fu, *, iostat=rc) line
                if (rc /= 0) exit

                do x = 1, width
                    if (line(x:x) == '#') world(x, y) = .true.
                end do
            end do
        end if

        close (fu)
    end subroutine read_map

    subroutine output(world, width, height)
        !! Outputs current state of world.
        character(len=1), parameter         :: space = '.'
        character(len=1), parameter         :: cell  = '#'
        character(len=5), parameter         :: color = ANSI_ESC // '[32m'
        character(len=5), parameter         :: reset = ANSI_ESC // '[39m'
        logical, allocatable, intent(inout) :: world(:, :)
        integer,              intent(in)    :: width
        integer,              intent(in)    :: height
        integer                             :: x, y

        do y = 1, height
            do x = 1, width
                if (.not. world(x, y)) then
                    write (*, '(a)', advance='no') space
                else
                    write (*, '(3a)', advance='no') color, cell, reset
                end if
            end do

            write (*, *)
        end do
    end subroutine output

    subroutine next(world, buffer, width, height)
        !! Does next iteration.
        logical, allocatable, intent(inout) :: world(:, :)
        logical, allocatable, intent(inout) :: buffer(:, :)
        integer,              intent(in)    :: width
        integer,              intent(in)    :: height
        integer                             :: i, j, n, nx, ny, x, y
        logical                             :: cell

        buffer = .false.

        do concurrent (y = 1:height)
            do concurrent (x = 1:width)
                cell = world(x, y)
                n    = 0

                do concurrent (j = y - 1:y + 1)
                    do concurrent (i = x - 1:x + 1)
                        if (i == x .and. j == y) cycle

                        nx = 1 + modulo(i - 1, width)
                        ny = 1 + modulo(j - 1, height)

                        if (world(nx, ny)) n = n + 1
                    end do
                end do

                ! New cell is born.
                if (.not. cell .and. n == 3) &
                    buffer(x, y) = .true.
                ! Cell stays alive.
                if (cell .and. (n == 2 .or. n == 3)) &
                    buffer(x, y) = .true.
            end do
        end do

        world = buffer
    end subroutine next

    subroutine cls()
        print '(2a)', ANSI_ESC, '[2J'
    end subroutine cls

    subroutine reset_cursor()
        print '(2a)', ANSI_ESC, '[0;0H'
    end subroutine reset_cursor

    subroutine hide_cursor()
        print '(2a)', ANSI_ESC, '[?25l'
    end subroutine hide_cursor

    subroutine show_cursor()
        print '(2a)', ANSI_ESC, '[?25h'
    end subroutine show_cursor
end program life
