module drawing_functions
    use iso_fortran_env
    implicit none

    type Canvas
        real(REAL64)   :: box_size(3)
        integer(INT32) :: box_size_voxels(3)
        real(REAL64), allocatable :: nodes(:,:,:)
    contains
        procedure :: place
    end type

    type DrawingHandler
        real(REAL64) :: origin(3)
    contains    
        procedure :: cuboid
        procedure :: spheroid
    end type

    type(DrawingHandler) :: drawing

    type SymetryOperation
    contains
        procedure :: rotation
        procedure :: hexagonal
    end type

    type(SymetryOperation) :: operation


contains

    subroutine place(self, material, pos)
        
        class(Canvas) :: self
        character(*) :: material
        integer(INT32) :: pos(3)
        integer(INT32) :: i,j,k
        i = pos(1); j = pos(2); k = pos(3)

        select case(material)
        case('material_1')
            self % nodes(i,j,k) = self % nodes(i,j,k) + 0.5
        case('material_2')
            self % nodes(i,j,k) = self % nodes(i,j,k) + 0.2
        end select
    end subroutine

    subroutine cuboid(self, SL, material, origin, a, b, c)

        real(REAL64), parameter :: Lx = 15.0_REAL64, &
                                   Ly = 15.0_REAL64, &
                                   Lz = 15.0_REAL64
        class(DrawingHandler) :: self
        type(Canvas) :: SL
        character(*) :: material
        real(REAL64) :: origin(3), posit(3)
        real(REAL64) :: a, b, c
        integer(INT32) :: i,j,k
        
        do i=1,SL % box_size_voxels(1)
            posit(1) = origin(1) - (real(i)/Lx) 
            if(posit(1) >= -a/2 .and. posit(1) <= a/2) then
                if (b == 0.0) then
                    call SL % place(material, [i,1,1])
                else
                    do j=1,SL % box_size_voxels(2)
                        posit(2) = origin(2) - (real(j)/Ly)
                        if (posit(2) >= -b/2 .and. posit(2) <= b/2) then
                            if (c == 0.0)  then
                                call SL % place(material, [i,j,1])
                            else
                                do k=1,SL % box_size_voxels(3)
                                    posit(3) = origin(3) - (real(k)/Lz)
                                    if(posit(3) >= -c/2 .and. posit(3) <= c/2) then
                                        call SL % place(material, [i,j,k])
                                    end if
                                end do
                            end if
                        end if
                    end do
                end if
            end if
        end do
    end subroutine


    subroutine spheroid(self, SL, material, origin, a, b, c)
        
        real(REAL64), parameter :: Lx = 20.0_REAL64, &
                                   Ly = 20.0_REAL64, &
                                   Lz = 20.0_REAL64
        class(DrawingHandler) :: self
        type(Canvas) :: SL
        character(*) :: material
        real(REAL64) :: origin(3), posit(3)
        real(REAL64) :: a, b, c, y, z
        integer(INT32) :: i,j,k
        
        do i=1,SL % box_size_voxels(1)
            posit(1) = origin(1) - (real(i)/Lx) 
            if(posit(1) >= -a/2 .and. posit(1) <= a/2) then
                if (b == 0.0) then
                    call SL % place(material, [i,1,1])
                else
                    do j=1,SL % box_size_voxels(2)
                        posit(2) = origin(2) - (real(j)/Ly)
                        y = b*sqrt(1.0-(posit(1)/a)**2)
                        if (posit(2) >= -y/2 .and. posit(2) <= y/2) then
                            if (c == 0.0)  then
                                call SL % place(material, [i,j,1])
                            else
                                do k=1,SL % box_size_voxels(3)
                                    posit(3) = origin(3) - (real(k)/Lz)
                                    z = c*sqrt(1.0 - (posit(2)/b)**2 - (posit(1)/a)**2)
                                    if(posit(3) >= -z/2 .and. posit(3) <= z/2) then
                                        call SL % place(material, [i,j,k])
                                    end if
                                end do
                            end if
                        end if
                    end do
                end if
            end if
        end do
    end subroutine

    subroutine rotation(self, N, angle, file_in, file_out)

        class(SymetryOperation) :: self
        character(*) :: file_in, file_out 
        integer(INT32) :: N, angle
        integer :: i
        real(REAL64), allocatable :: lat_in(:,:), lat_out(:,:)
        real(REAL64) :: theta, pi = 3.14159265359

        allocate (lat_in(1:3, 1:N))
        allocate (lat_out(1:3, 1:N))

        theta = pi*(angle/180.0)

        open(11, file = file_in)
        open(12, file = file_out)

        do i=1, N
            read(11, *) lat_in(1, i), lat_in(2, i), lat_in(3, i)

            lat_out(1, i) = cos(theta)*lat_in(1, i) + sin(theta)*lat_in(2, i)
            lat_out(2, i) = - sin(theta)*lat_in(1, i) + cos(theta)*lat_in(2, i)
            lat_out(3, i) = lat_in(3, i)
                
            write (12, *) lat_out(1, i), lat_out(2, i), lat_out(3, i)
        end do

        close(11)
        close(12)

    end subroutine

    subroutine hexagonal(self, SL, Lx, Ly, Lz)

        class(SymetryOperation) :: self
        type(Canvas) :: SL
        integer(INT32) :: Lx, Ly, Lz
        integer :: i, j, k
        real :: i_f, j_f

        open(11, file = 'rede01.txt')
        do i=1,Lx
            do j=1,Ly
                do k=1,Lz
                    if (SL % nodes (i, j, k) /= 0) then
                        i_f = real(i) - (0.5)*real(j) - 1.0
                        j_f = (sqrt(3.0)/2.0)*real(j) - 1.0
                        write (11, *) i_f, j_f, k
                    end if
                end do
            end do
        end do
        close(11)

    end subroutine

    integer function count(SL, Lx, Ly, Lz)
    implicit none
    type(Canvas) :: SL
    integer(INT32) :: Lx, Ly, Lz, temp
    integer :: i, j, k
    
    temp = 0

    do i=1,Lx
        do j=1,Ly
            do k=1,Lz
                if (SL % nodes (i, j, k) /= 0) then
                    temp = temp + 1
                end if
            end do
        end do
    end do
    
    count = temp
    end function count

end module

program main
    use drawing_functions
    use iso_fortran_env
    implicit none
    integer(INT32), parameter :: Lx=15, &
                                 Ly=15, &
                                 Lz=15
    real(REAL64) :: origin(3) = [0.5_REAL64, 0.5_REAL64, 0.5_REAL64]
    real(REAL64) :: bounding_box(3) = [1.0_REAL64, 1.0_REAL64, 1.0_REAL64]
    type(Canvas) :: SL
    integer :: N
    ! Size of all space in real world units (angstrons for e.g.)
    SL % box_size = bounding_box
    ! Size of all space in voxels (like a pixel but general)
    SL % box_size_voxels = [Lx,Ly,Lz]
    ! Allocate the canvas in voxels and init. with zeros
    allocate(SL % nodes(Lx,Ly,Lz))
    SL % nodes = 0.0_REAL64

    
    call drawing % cuboid(SL, "material_1", origin,       &
                                            a=0.8_REAL64, &
                                            b=0.8_REAL64, &
                                            c=0.8_REAL64)

    N = count(SL, Lx, Ly, Lz)

    call operation % hexagonal(SL, Lx, Ly, Lz)    
   
    call operation % rotation(N, 120, 'rede01.txt', 'rede02.txt')

    call operation % rotation(N, 240, 'rede01.txt', 'rede03.txt')

end program
