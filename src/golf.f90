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
    real,             parameter :: VERSION    = 0.4

    logical, allocatable :: world(:, :)     ! World map.
    logical, allocatable :: buffer(:, :)    ! Buffered world map.
    character(len=100)   :: file_name = ''  ! File name of the stored world.
    integer              :: rows      = 10  ! Rows (default: 10)
    integer              :: columns   = 30  ! Columns (default: 30)
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
        rc = c_usleep(SLEEP_TIME)
    end do

    ! Quit.
    call show_cursor()
    call clean()
contains
    logical function get_cell(x, y)
        !! Returns cell of world.
        integer, intent(in) :: x
        integer, intent(in) :: y
        integer             :: i, j

        ! We are on a torus.
        i = mod(x, columns - 1) + 1
        j = mod(y, rows - 1) + 1

        get_cell = world(i, j)
    end function get_cell

    integer function nneighbours(x, y)
        !! Counts surrounding neighbours of field with given coordinates.
        !! Instead of do loops one can also write:
        !!
        !!     forall (i = x - 1:x + 1, j = y - 1:y + 1, i /= j .and. get_cell(i, j)) &
        !!         nneighbours = nneighbours + 1
        !!
        !! (Of course, `get_cell()` must be pure.)
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
        type(option_type) :: opts(5)

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
        character(len=100), intent(in) :: file_name
        character(len=columns)         :: line
        integer                        :: fu, rc, x, y

        if (len_trim(file_name) == 0) then
            write (stderr, '(a)') 'Invalid file name'
            stop
        end if

        open (newunit=fu, file=file_name, action='read', iostat=rc)

        if (rc /= 0) then
            write (stderr, '(3a, i0)') 'Reading file "', trim(file_name), '" failed: error ', rc
            stop
        else
            do y = 1, rows
                read (fu, *, iostat=rc) line
                if (rc /= 0) exit

                do x = 1, columns
                    if (line(x:x) == '#') world(x, y) = .true.
                end do
            end do
        end if

        close (fu)
    end subroutine read_map

    subroutine init_world()
        !! Initialises the world.
        allocate (world(columns, rows))
        allocate (buffer(columns, rows))
    end subroutine init_world

    subroutine output()
        !! Outputs current state of world.
        character(len=1), parameter :: space = '.'
        character(len=1), parameter :: cell  = '#'
        character(len=5), parameter :: color = ANSI_ESC // '[32m'
        character(len=5), parameter :: reset = ANSI_ESC // '[39m'
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
        integer, intent(in) :: x
        integer, intent(in) :: y
        logical, intent(in) :: cell
        integer             :: i, j

        ! We are on a torus.
        i = mod(x, columns - 1) + 1
        j = mod(y, rows - 1) + 1

        buffer(i, j) = cell
    end subroutine set_cell

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
