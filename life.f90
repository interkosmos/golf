! life.f90
!
! CONWAY'S GAME OF LIFE
!
! Author:  Philipp Engel
! Date:    2017-DEC-22
! Licence: ISC
program life
    use iso_c_binding
    use iso_fortran_env
    implicit none

    integer, parameter :: u       = selected_char_kind('ISO_10646')
    integer, parameter :: LINES   = 10
    integer, parameter :: COLS    = 30
    real,    parameter :: VERSION = 0.1

    logical, dimension(LINES, COLS) :: world   = .false.                ! world map
    logical, dimension(LINES, COLS) :: buffer  = .false.                ! buffered world map
    integer(c_int32_t)              :: t       = int(250000, c_int32_t) ! sleep time of 0.25 seconds
    integer                         :: gen     = 0                      ! current generation
    integer                         :: max_gen = 60                     ! maximum number of generations
    integer                         :: i

    interface
        subroutine usleep(useconds) bind(C)
            use iso_c_binding
            implicit none
            integer(c_int32_t), value :: useconds
        end subroutine
    end interface

    ! set output to UTF-8
    open(unit=output_unit, encoding='utf-8')

    ! get command-line arguments
    call read_arguments()

    ! init everything
    call init_world()
    call hide_cursor()
    call clear_screen()
    call reset_cursor()
    call display_intro()
    call display_world()

    do i = 1, max_gen
        call next()
        call reset_cursor()
        call display_intro()
        call display_world()
        call display_generation()
        call swap()

        call usleep(t)
    end do

    call show_cursor()
    call exit(0)

    contains
        subroutine read_arguments()
            use f90getopt
            implicit none
            type(option_s) :: opts(2)

            opts(1) = option_s('generations', .true.,  'g')
            opts(2) = option_s('version',     .false., 'v')

            do
                select case(getopt('g:v', opts))
                    case (char(0))
                        exit
                    case ('g')
                        read(optarg, '(i3)') max_gen
                    case ('v')
                        write(*, '(a, f3.1)') 'Game of Life ', VERSION
                        call exit(0)
                end select
            end do
        end subroutine read_arguments

        subroutine init_world()
            !! Initialises the world.
            world(5, 15)    = .true.
            world(6, 16)    = .true.
            world(7, 14:16) = .true.

            world(8, 4)     = .true.
            world(9, 5)     = .true.
            world(10, 3:5)  = .true.

            world(2:6, 3:8) = .true.
        end subroutine init_world

        subroutine display_world()
            !! Outputs current state of world.
            implicit none
            character(len=1), parameter :: space = '.'
            character(len=1), parameter :: cell  = '#'
            character(len=5), parameter :: color = '\u001b[32m'
            character(len=5), parameter :: reset = '\u001b[39m'
            integer                     :: x, y

            do y = 1, LINES
                do x = 1, COLS
                    if (.not. world(y, x)) then
                        write(*, '(a)', advance='no') space
                    else
                        write(*, '(a)', advance='no') color // cell // reset
                    end if
                end do

                write(*, '(a)', advance='no') u_'\n'
            end do
        end subroutine display_world

        subroutine display_generation()
            !! Outputs current generation.
            write(*, '(a, i0, a, i0)') '\nGeneration: ', gen, '/', max_gen
        end subroutine display_generation

        subroutine next()
            !! Does next iteration.
            integer :: neighbours
            integer :: x, y

            do y = 1, LINES
                do x = 1, COLS
                    neighbours = count_neighbours(x, y)

                    ! new cell is born
                    if (.not. world(y, x) .and. neighbours == 3) &
                        call set(x, y, .true.)
                    ! cell dies of loneliness
                    if (world(y, x) .and. neighbours < 2) &
                        call set(x, y, .false.)
                    ! cell stays alive
                    if (world(y, x) .and. (neighbours == 2 .or. neighbours == 3)) &
                        call set(x, y, .true.)
                    ! cells dies of overcrowding
                    if (world(y, x) .and. neighbours > 3) &
                        call set(x, y, .false.)
                end do
            end do

            gen = gen + 1
        end subroutine next

        subroutine swap()
            world = buffer
            buffer = .false.
        end subroutine swap

        subroutine clear_screen()
            write(*, '(a)') u_'\u001b[2J'
        end subroutine clear_screen

        subroutine reset_cursor()
            write(*, '(a)') u_'\u001b[0;0H'
        end subroutine reset_cursor

        subroutine hide_cursor()
            write(*, '(a)') u_'\u001b[?25l'
        end subroutine hide_cursor

        subroutine show_cursor()
            write(*, '(a)') u_'\u001b[?25h'
        end subroutine show_cursor

        subroutine display_intro()
            write (*, '(a)') "CONWAY'S GAME OF LIFE\n"
        end subroutine display_intro

        subroutine set(x, y, cell)
            !! Sets single field in buffered world.
            implicit none
            integer, intent(in) :: x
            integer, intent(in) :: y
            logical, intent(in) :: cell
            integer             :: i, j

            i = x
            j = y

            ! we are on a torus
            if (x == 0)         i = COLS
            if (x == COLS + 1)  i = 1
            if (y == 0)         j = LINES
            if (y == LINES + 1) j = 1

            buffer(j, i) = cell
        end subroutine set

        function get(x, y) result(cell)
            !! Returns cell of world.
            implicit none
            integer, intent(in) :: x
            integer, intent(in) :: y
            integer             :: i, j
            logical             :: cell

            i = x
            j = y

            ! we are on a torus
            if (x == 0)         i = COLS
            if (x == COLS + 1)  i = 1
            if (y == 0)         j = LINES
            if (y == LINES + 1) j = 1

            cell = world(j, i)
        end function get

        function count_neighbours(x, y) result(neighbours)
            !! Returns number of neighbours of field with given coordinates.
            implicit none
            integer, intent(in) :: x
            integer, intent(in) :: y
            integer             :: i, j
            integer             :: neighbours

            neighbours = 0

            do i = x - 1, x + 1
                do j = y - 1, y + 1
                    if ((i /= x .or. j /= y) .and. get(i, j)) &
                        neighbours = neighbours + 1
                end do
            end do
        end function count_neighbours
end program life