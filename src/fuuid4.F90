!> Create a random (version 4) Universally Unique Identifier (UUID)
!>
!> A UUID looks like "4042E716-2556-4715-90F0-C6518463B4E5" and is a random
!> 128-bit number. This is sufficiently random that we can except zero
!> collisions in any relevant timeframe, and so is useful for uniquely
!> identifing simulations, for example.
!>
!> If compiled with the preprocessor macro `FUUID4_HAS_LIBUUID > 0`, then it
!> uses a wrapper around the C `libuuid` library (which must be
!> linked). Otherwise, uses a fallback random number generator.
!>
!> This module contains its own implementation of the
!> [MT19937](https://en.wikipedia.org/wiki/Mersenne_Twister) psuedo-random
!> number generator (PRNG) so that it doesn't intefere with the PRNG state used
!> in the rest of the program. This implementation originally written by
!> R. Numata, 2010, from a C program by Makoto Matsumoto and Takuji Nishimura,
!> 1997
module fuuid4
  use, intrinsic :: iso_c_binding, only: c_signed_char
  implicit none

  private

  public :: generate_uuid, uuid_len

  !> Length of the UUID character
  integer, parameter :: uuid_len = 36

#if !FUUID4_HAS_LIBUUID
  !> Default seed for mt19937
  integer, parameter :: default_seed = 4357

  ! Period parameters
  integer, parameter :: N_mt19937 = 624 !< state size
  integer, parameter :: M_mt19937 = 397 !< state size

  !> Mersenne-Twister object. Allows multiple, independent instances
  !> to be used within a program
  type mt19937_type
    integer :: mt(0:N_mt19937-1) !< The array for the state vector
    integer :: mti = N_mt19937+1 !< mti==N+1 means mt[N] is not initialized
  contains
    procedure :: set_seed
    procedure :: generate
  end type mt19937_type
#endif

contains

  !> Convert a 1-byte integer into hexadecimal
  elemental function c_signed_char_to_hex(char) result(hex)
    integer(c_signed_char), intent(in) :: char
    character(len=2) :: hex
    ! Note this almost certainly writes the result in uppercase
    write(hex, '(z2.2)') char
  end function c_signed_char_to_hex

  !> Convert an array of 1-byte integers into UUID format:
  !> "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
  function format_uuid(uuid_in)
    character(len=uuid_len) :: format_uuid
    character(2), dimension(0:15) :: uuid_as_hex
    character(32) :: uuid_unformatted
    integer(c_signed_char), dimension(16), intent(in) :: uuid_in
    integer :: i

    uuid_as_hex = c_signed_char_to_hex(uuid_in)

    do i = 0, 15
      uuid_unformatted(2*i+1:2*i+2) = uuid_as_hex(i)
    end do

    format_uuid = &
         uuid_unformatted(1:8) // "-" // uuid_unformatted(9:12) // "-" // &
         uuid_unformatted(13:16) // "-" // uuid_unformatted(17:20) // "-" //&
         uuid_unformatted(21:)
  end function format_uuid

  !> Generate a version 4 UUID using either the libuuid library, or
  !> our own random number generator wrapper
  function generate_uuid()
    character(len=uuid_len) :: generate_uuid
    integer(c_signed_char), dimension(16) :: uuid_version_4

#if FUUID4_HAS_LIBUUID
    ! Use a wrapper for the C library libuuid
    interface
      subroutine uuid_generate_c(uuid_out) bind(C, name="uuid_generate")
        import
        integer(c_signed_char), dimension(16) :: uuid_out
      end subroutine uuid_generate_c
    end interface

    call uuid_generate_c(uuid_version_4)
#else
    type(mt19937_type) :: prng
    integer :: i

    ! Always re-start the PRNG
    call prng%set_seed(get_random_seed())

    ! Generate random numbers between 0 and 255
    do i = 1, size(uuid_version_4)
      uuid_version_4(i) = int(prng%generate() * huge(uuid_version_4), kind=c_signed_char)
    end do

    ! Version must be 0100xxxx (i.e. 0x4X)
    uuid_version_4(7) = iand(uuid_version_4(7), 79_c_signed_char)
    uuid_version_4(7) = ior(uuid_version_4(7), 64_c_signed_char)

    ! Variant must be 10xxxxxx (i.e 0x8X - 0xBX)
    uuid_version_4(9) = iand(uuid_version_4(9), -65_c_signed_char)
    uuid_version_4(9) = ior(uuid_version_4(9), -127_c_signed_char)
#endif
    generate_uuid = format_uuid(uuid_version_4)
  end function generate_uuid

#if !FUUID4_HAS_LIBUUID
  !> Uses a method for getting a nice seed for the PRNG taken from
  !> [gfortran
  !> documentation](https://gcc.gnu.org/onlinedocs/gcc-6.4.0/gfortran/RANDOM_005fSEED.html)
  function get_random_seed() result(seed)
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer :: seed

    integer(int64) :: time
    integer :: time_values(8)
    ! Convert from milliseconds to larger units
    integer, parameter :: second = 1000
    integer, parameter :: minute = 60*second
    integer, parameter :: hour = 60*minute
    integer(int64), parameter :: day = 24*hour
    integer(int64), parameter :: month = 31*day
    integer(int64), parameter :: year = 365_int64*day

    integer :: un
    integer :: istat

    ! First try if the OS provides a random number generator
    open(newunit=un, file="/dev/urandom", access="stream", &
         form="unformatted", action="read", status="old", iostat=istat)
    if (istat == 0) then
      read(un) seed
      close(un)
    else
      ! Fallback to using current system time
      call system_clock(time)
      if (time == 0) then
        ! It's plausible the system_clock isn't POSIX/epoch time so
        ! let's try a different method to get the time
        call date_and_time(values=time_values)
        time = (time_values(1) - 1970)*year &
             + time_values(2)*month &
             + time_values(3)*day &
             + time_values(5)*hour &
             + time_values(6)*minute &
             + time_values(7)*second &
             + time_values(8)
      end if

      ! Ideally here we'd also XOR the PID, which would help when
      ! launching multiple processes at the same time. Note that if
      ! you need a consistent UUID across MPI ranks, you'll need
      ! another function to generate one UUID and broadcast that

      ! One step of linear congruential generator
      time = mod(time, 4294967296_int64)
      time = mod(time*279470273_int64, 4294967291_int64)
      seed = int(mod(time, int(huge(0), int64)), kind(seed))
    end if
  end function get_random_seed

  !> Set initial seed of a [[mt19937_type]] instance
  !>
  !> Set initial seeds to mt[N] using the generator Line 25 of Table 1
  !> in [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.),
  !> pp102]
  subroutine set_seed(this, seed)
    class(mt19937_type), intent(inout) :: this
    integer, intent(in) :: seed
    this%mt(0) = iand(seed, -1)
    associate(mti => this%mti)
      do mti = 1, N_mt19937 - 1
        this%mt(mti) = iand(69069 * this%mt(mti - 1), -1)
      end do
    end associate
    ! Explicitly make sure this is set, as loop index may be undefined
    this%mti = N_mt19937
  end subroutine set_seed

  !> Generate a random number from a [[mt19937_type]]
  function generate(this)
    class(mt19937_type), intent(inout) :: this
    real :: generate

    ! Period parameters
    integer, parameter :: LMASK = 2147483647 ! least significant r bits
    integer, parameter :: UMASK = -LMASK - 1 ! most significant w-r bits
    integer, parameter :: MATA = -1727483681 !< constant vector a
    integer, parameter :: mag01(0:1) = [0, MATA] ! mag01(x) = x * MATA for x=0,1

    ! Tempering parameters
    integer, parameter :: TMASKB = -1658038656
    integer, parameter :: TMASKC = -272236544

    real, parameter :: pow = 2.**32       ! range of possible `integer` values
    real, parameter :: div = 1./pow       ! divided by 2**32 [0,1)-real-interval
    real :: random_temp
    integer :: y, kk

    if (this%mti >= N_mt19937) then ! generate N words at one time
       if (this%mti == N_mt19937+1) then ! if set_seed() has not been called,
          call this%set_seed(default_seed) ! a default initial seed is used
       endif

      do kk = 0, N_mt19937 - M_mt19937 - 1
        y = ior(iand(this%mt(kk), UMASK), iand(this%mt(kk + 1), LMASK))
        this%mt(kk) = ieor(ieor(this%mt(kk + M_mt19937), ishft(y, -1)), mag01(iand(y, 1)))
      end do

      do kk = N_mt19937 - M_mt19937, N_mt19937 - 2
        y = ior(iand(this%mt(kk), UMASK), iand(this%mt(kk + 1), LMASK))
        this%mt(kk) = ieor(ieor(this%mt(kk + (M_mt19937 - N_mt19937)), ishft(y, -1)), mag01(iand(y, 1)))
      end do

      y = ior(iand(this%mt(N_mt19937 - 1), UMASK), iand(this%mt(0), LMASK))
      this%mt(N_mt19937 - 1) = ieor(ieor(this%mt(M_mt19937 - 1), ishft(y, -1)), mag01(iand(y, 1)))
      this%mti = 0
    end if

    y = this%mt(this%mti)
    this%mti = this%mti + 1
    y = ieor(y, ishft(y, -11))
    y = ieor(y, iand(ishft(y, 7), TMASKB))
    y = ieor(y, iand(ishft(y, 15), TMASKC))
    y = ieor(y, ishft(y, -18))

    random_temp = real(y)
    if (random_temp < 0.) random_temp = random_temp + pow
    generate = random_temp*div
  end function generate
#endif

end module fuuid4
