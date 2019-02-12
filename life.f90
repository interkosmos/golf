! life.f90
!
! Conway's Game of Life, written in Fortran 2003 for Unix/Linux.
! The world can be loaded from a text file, for instance:
!
! $ ./life --file world.txt --columns 30 --rows 10
!
! Author:  Philipp Engel
! Licence: ISC
!
! Compilation:
!
! $ mkdir build && cd build/
! $ cmake -DCMAKE_Fortran_COMPILER=gfortran8 -DCMAKE_INSTALL_RPATH=/usr/local/lib/gcc8 ..
! $ make
!
! Command-line Arguments:
!
!   -v, --version
!       Game version.
!
!   -g, --generations
!       Number of generations to run (default: 60).
!
!   -f, --file
!       File name of world to load.
!
!   -c, --columns
!       Columns of the world (default: 30).
!
!   -r, --rows
!       Rows of the world (default: 10).
!
program life
    use, intrinsic :: iso_fortran_env, only: stdout => output_unit, stderr => error_unit
    use, intrinsic :: iso_c_binding, only: c_int32_t
    implicit none
    external :: sigint_handler

    character(len=*), parameter :: ESC     = achar(27)
    integer,          parameter :: T       = 250 * 1000
    real,             parameter :: VERSION = 0.3

    logical, allocatable :: world(:,:)      ! World map.
    logical, allocatable :: buffer(:,:)     ! Buffered world map.
    character(len=100)   :: file_name = ""  ! File name of the stored world.
    integer              :: rows      = 10  ! Rows (default: 10)
    integer              :: columns   = 30  ! Columns (default: 30)
    integer              :: max_gen   = 60  ! Maximum number of generations (default: 60).
    integer              :: gen             ! Current generation.

    interface
        subroutine usleep(useconds) bind(c)
            !! Interface to `usleep()` in libc.
            use, intrinsic :: iso_c_binding
            implicit none
            integer(c_int32_t), value :: useconds
        end subroutine
    end interface

    ! Catch SIGINT.
    call signal(2, sigint_handler)

    ! Set output to UTF-8.
    open (unit=stdout, encoding='utf-8')

    ! Init everything.
    call read_arguments()
    call init_world()
    call read_map(file_name)
    call hide_cursor()
    call cls()

    ! Main loop.
    do gen = 1, max_gen
        call reset_cursor()
        print '(a, /)', "CONWAY'S GAME OF LIFE"
        call output()
        print '(/, a, i0, a, i0)', 'Generation: ', gen, '/', max_gen

        call next()
        call microsleep(T)
    end do

    ! Quit.
    call show_cursor()
    call clean()
contains
    logical function get_cell(x, y)
        !! Returns cell of world.
        implicit none
        integer, intent(in) :: x
        integer, intent(in) :: y
        integer             :: i, j

        i = x
        j = y

        ! We are on a torus.
        if (x == 0)           i = columns
        if (x == columns + 1) i = 1
        if (y == 0)           j = rows
        if (y == rows + 1)    j = 1

        get_cell = world(i, j)
    end function get_cell

    integer function nneighbours(x, y)
        !! Counts surrounding neighbours of field with given coordinates.
        implicit none
        integer, intent(in) :: x
        integer, intent(in) :: y
        integer             :: i, j

        nneighbours = 0

        do j = y - 1, y + 1
            do i = x - 1, x + 1
                if (i == x .and. j == y) &
                   cycle

                if (get_cell(i, j)) &
                    nneighbours = nneighbours + 1
            end do
        end do
    end function nneighbours

    subroutine microsleep(us)
        !! Wrapper for `usleep()`.
        use, intrinsic :: iso_c_binding, only: c_int32_t
        implicit none
        integer, intent(in) :: us

        call usleep(int(us, kind=c_int32_t))
    end subroutine microsleep

    subroutine clean()
        !! Cleans up memory.
        if (allocated(world)) &
            deallocate (world)

        if (allocated(buffer)) &
            deallocate (buffer)
    end subroutine clean

    subroutine read_arguments()
        !! Reads command-line arguments.
        use :: f90getopt
        implicit none
        type(option_s) :: opts(5)

        opts(1) = option_s('rows',        .true.,  'r')
        opts(2) = option_s('columns',     .true.,  'c')
        opts(3) = option_s('file',        .true.,  'f')
        opts(4) = option_s('generations', .true.,  'g')
        opts(5) = option_s('version',     .false., 'v')

        do
            select case (getopt('l:c:f:g:v', opts))
                case (char(0))
                    exit
                case ('l')
                    read (opt_arg, '(i3)') rows
                case ('c')
                    read (opt_arg, '(i3)') columns
                case ('f')
                    read (opt_arg, '(a)') file_name
                case ('g')
                    read (opt_arg, '(i3)') max_gen
                case ('v')
                    print '(a, f3.1)', 'Game of Life ', version
                    call exit(0)
            end select
        end do
    end subroutine read_arguments

    subroutine read_map(file_name)
        !! Loads world from text file.
        implicit none
        character(len=100), intent(in) :: file_name
        integer, parameter             :: U = 10
        character(len=columns)         :: line
        integer                        :: rc, x, y

        if (len_trim(file_name) == 0) then
            write (stderr, '(a)') 'Invalid file name'
            stop
        end if

        open (unit=U, file=file_name, action='read', iostat=rc)

        if (rc == 0) then
            do y = 1, rows
                read (U, *, iostat=rc) line
                if (rc /= 0) exit

                do x = 1, columns
                    if (line(x:x) == '#') &
                        world(x, y) = .true.
                end do
            end do
        else
            write (stderr, '(3a, i0)') 'Reading file "', trim(file_name), '" failed: error ', rc
            stop
        end if

        close (U)
    end subroutine read_map

    subroutine init_world()
        !! Initialises the world.
        implicit none
        integer :: rc

        allocate (world(columns, rows), stat=rc)
        allocate (buffer(columns, rows), stat=rc)
    end subroutine init_world

    subroutine output()
        !! Outputs current state of world.
        implicit none
        character(len=1), parameter :: space = '.'
        character(len=1), parameter :: cell  = '#'
        character(len=5), parameter :: color = ESC // '[32m'
        character(len=5), parameter :: reset = ESC // '[39m'
        integer                     :: x, y

        do y = 1, rows
            do x = 1, columns
                if (.not. get_cell(x, y)) then
                    write (*, '(a)', advance='no') space
                else
                    write (*, '(3a)', advance='no') color, cell, reset
                end if
            end do

            write (*, *)
        end do
    end subroutine output

    subroutine next()
        !! Does next iteration.
        implicit none
        integer :: n, x, y
        logical :: cell

        buffer = .false.

        do y = 1, rows
            do x = 1, columns
                cell = get_cell(x, y)
                n    = nneighbours(x, y)

                ! New cell is born.
                if (.not. cell .and. n == 3) &
                    call set_cell(x, y, .true.)
                ! Cell stays alive.
                if (cell .and. (n == 2 .or. n == 3)) &
                    call set_cell(x, y, .true.)
            end do
        end do

        world = buffer
    end subroutine next

    subroutine set_cell(x, y, cell)
        !! Sets single field in buffered world.
        implicit none
        integer, intent(in) :: x
        integer, intent(in) :: y
        logical, intent(in) :: cell
        integer             :: i, j

        i = x
        j = y

        ! We are on a torus.
        if (x == 0)           i = columns
        if (x == columns + 1) i = 1
        if (y == 0)           j = rows
        if (y == rows + 1)    j = 1

        buffer(i, j) = cell
    end subroutine set_cell

    subroutine cls()
        print '(2a)', ESC, '[2J'
    end subroutine cls

    subroutine reset_cursor()
        print '(2a)', ESC, '[0;0H'
    end subroutine reset_cursor

    subroutine hide_cursor()
        print '(2a)', ESC, '[?25l'
    end subroutine hide_cursor

    subroutine show_cursor()
        print '(2a)', ESC, '[?25h'
    end subroutine show_cursor
end program life

subroutine sigint_handler()
    print '(2a)', achar(27), '[?25h'
    stop
end subroutine sigint_handler
