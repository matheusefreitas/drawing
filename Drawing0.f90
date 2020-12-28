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
        !! FIXME add new functions here
        procedure :: cuboid
    end type

    type(DrawingHandler) :: drawing

contains

    subroutine place(self, material, pos)
        !! This functions is a placeholder just for testing
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
        class(DrawingHandler) :: self
        type(Canvas) :: SL
        character(*) :: material
        real(REAL64) :: origin(3)
        real(REAL64) :: a, b, c
        integer(INT32) :: i,j,k
        !!! FIXME LOGIC GOES HERE
        do i=1,SL % box_size_voxels(1)
            do j=1,SL % box_size_voxels(2)
                do k=1,SL % box_size_voxels(3)
                    call SL % place(material, [i,j,k])
                end do
            end do
        end do
    end subroutine
end module

program main
    use drawing_functions
    use iso_fortran_env
    implicit none
    integer(INT32), parameter :: Lx=100, &
                                 Ly=100, &
                                 Lz=100
    real(REAL64) :: origin(3) = [0.0_REAL64, 0.0_REAL64, 0.0_REAL64]
    real(REAL64) :: bounding_box(3) = [1.0_REAL64, 1.0_REAL64, 1.0_REAL64]
    type(Canvas) :: SL
    integer :: i, j, k
    ! Size of all space in real world units (angstrons for e.g.)
    SL % box_size = bounding_box
    ! Size of all space in voxels (like a pixel but general)
    SL % box_size_voxels = [Lx,Ly,Lz]
    ! Allocate the canvas in voxels and init. with zeros
    allocate(SL % nodes(Lx,Ly,Lz))
    SL % nodes = 0.0_REAL64

    ! Some example drawing function
    call drawing % cuboid(SL, "material_1", origin,       &
                                            a=0.5_REAL64, &
                                            b=0.5_REAL64, &
                                            c=1.0_REAL64)
    ! Shows the resultant figure
    print '("| i | j | k | color |")'
    do i=1,Lx
        do j=1,Ly
            do k=1,Lz
                print*, i,j,k,SL % nodes(i,j,k)
            end do
        end do
    end do
end program
