! getopt.f90
!
! getopt()- and getopt_long()-like functionality for Fortran 90.
! Based on sources from Mark Gates.
!
! GNU General Public License, v.2.0
! Copyright (c) 2014, Hani Andreas Ibrahim
!
! GitHub: https://github.com/haniibrahim/f90getopt/
module f90getopt
    use, intrinsic :: iso_fortran_env, only: error_unit, output_unit
    implicit none

    character(len=80) :: opt_arg             ! Option's value.
    character         :: opt_opt             ! Option's character.
    integer           :: opt_ind = 1         ! Index of the next argument to process.
    logical           :: opt_err = .true.    ! Errors are printed by default. Set opt_err = .false. to suppress them.

    type :: option_type
        character(len=80) :: name           ! Name of the option.
        logical           :: has_arg        ! Option has an argument (.true./.false.).
        character         :: short          ! Option's short character equal to opt_opt.
    end type option_type

    ! grp_ind is index of next option within group; always >= 2.
    integer, private :: grp_ind = 2
contains
    ! Return str(i:j) if 1 <= i <= j <= len(str), else return empty string.
    ! This is needed because Fortran standard allows but doesn't *require*
    ! short-circuited logical AND and OR operators. So this sometimes fails:
    !     if (i < len(str) .and. str(i + 1:i + 1) == ':') then
    ! But this works:
    !     if (substr(str, i + 1, i + 1) == ':') then
    character function substr(str, i, j)
        character(len=*), intent(in) :: str
        integer,          intent(in) :: i, j

        if (1 <= i .and. i <= j .and. j <= len(str)) then
            substr = str(i:j)
        else
            substr = ''
        end if
    end function substr

    character function getopt(opt_string, long_opts)
        character(len=*),  intent(in)           :: opt_string
        type(option_type), intent(in), optional :: long_opts(:)
        character(len=80)                       :: arg

        opt_arg = ''

        if (opt_ind > command_argument_count()) then
            getopt = char(0)
        end if

        call get_command_argument(opt_ind, arg)

        if (present(long_opts) .and. arg(1:2) == '--') then
            getopt = process_long(long_opts, arg)
        else if (arg(1:1) == '-') then
            getopt = process_short(opt_string, arg)
        else
            getopt = char(0)
        end if
    end function getopt

    character function process_long(long_opts, arg)
        type(option_type), intent(in) :: long_opts(:)
        character(len=*),  intent(in) :: arg
        integer                       :: i
        integer                       :: len_arg
        logical                       :: has_equal_sign = .false.

        len_arg = len_trim(arg)

        ! Search for equal sign in arg and set flag "has_equal_sign" and
        ! length of arg (until equal sign).
        do i = 1, len_arg
            if (arg(i:i) == '=') then
                has_equal_sign = .true.
                len_arg        = i - 1
                exit
            end if
        enddo

        ! Search for matching long option.
        if (.not. has_equal_sign) then
            opt_ind = opt_ind + 1
        end if

        do i = 1, size(long_opts)
            if (arg(3:len_arg) == long_opts(i)%name) then
                opt_opt      = long_opts(i)%short
                process_long = opt_opt

                if (long_opts(i)%has_arg) then
                    if (has_equal_sign) then
                        ! Long option has equal sign between value and option.
                        if (arg(len_arg + 2:) == '') then
                            ! No value (len_arg+2 value after "=".
                            write (error_unit, '(3a)') 'Error: Option "', trim(arg), '" requires a value'
                            process_long = char(0) ! Option not valid.
                        else
                            call get_command_argument(opt_ind, opt_arg)
                            opt_arg = opt_arg(len_arg + 2:)
                            opt_ind = opt_ind + 1
                        end if
                    else
                        ! Long option has no equal sign between value and option.
                        if (opt_ind <= command_argument_count()) then
                            call get_command_argument(opt_ind, opt_arg)
                            opt_ind = opt_ind + 1
                        else if (opt_err) then
                            write (error_unit, '(3a)') 'Error: Option "', trim(arg), '" requires a value'
                            process_long = char(0) ! Option not valid.
                        end if
                    end if
                end if

                return
            end if
        end do

        ! Else not found.
        process_long = char(0)
        opt_opt      = '?'

        if (opt_err) &
            write (error_unit, '(3a)') 'Error: Unrecognized option "', arg(1:len_arg), '"'
    end function process_long

    character function process_short(opt_string, arg)
        character(len=*), intent(in) :: opt_string
        character(len=*), intent(in) :: arg
        integer                      :: i, arg_len

        arg_len       = len(trim(arg))
        opt_opt       = arg(grp_ind:grp_ind)
        process_short = opt_opt

        i = index(opt_string, opt_opt)

        if (i == 0) then
            ! Unrecognized option.
            process_short = '?'

            if (opt_err) then
                write (error_unit, '(3a)') 'Error: Unrecognized option "-', opt_opt, '"'
            end if
        end if

        if (i > 0 .and. substr(opt_string, i + 1, i + 1) == ':') then
            ! Required argument.
            opt_ind = opt_ind + 1

            if (arg_len > grp_ind) then
                ! -x arg, return remainder of arg.
                opt_arg = arg(grp_ind + 1:arg_len)
            else if (opt_ind <= command_argument_count()) then
                ! -x arg, return next arg.
                call get_command_argument(opt_ind, opt_arg)
                opt_ind = opt_ind + 1
            else if (opt_err) then
                write (error_unit, '(3a)') "Error: Option '-", opt_opt, "' requires a value"
                process_short = char(0) ! Option not valid.
            end if

            grp_ind = 2
        else if (arg_len > grp_ind) then
            ! No argument (or unrecognized), go to next option in argument (-xyz).
            grp_ind = grp_ind + 1
        else
            ! No argument (or unrecognized), go to next argument.
            grp_ind = 2
            opt_ind = opt_ind + 1
        end if
    end function process_short
end module f90getopt
