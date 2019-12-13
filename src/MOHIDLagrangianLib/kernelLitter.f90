    !------------------------------------------------------------------------------
    !        IST/MARETEC, Water Modelling Group, Mohid modelling system
    !        USC/GFNL, Group of NonLinear Physics, Mohid modelling system
    !------------------------------------------------------------------------------
    !
    ! TITLE         : Mohid Model
    ! PROJECT       : Mohid Lagrangian Tracer
    ! MODULE        : kernelLitter
    ! URL           : http://www.mohid.com
    ! AFFILIATION   : USC/MARETEC, Marine Modelling Group
    ! DATE          : September 2019
    ! REVISION      : Canelas 0.1
    !> @author
    !> Ricardo Birjukovs Canelas
    !
    ! DESCRIPTION:
    !> Defines an abstract physics kernel class for litter associated processes.
    !> This class has several methods, that should be designed on a one method - one
    !> process approach.
    !> The output of every kernel should be a 2D matrix, where a row represents the
    !> derivative of the state vector of a given tracer. n columns - n variables.
    !> This is the step were interpolation and physics actually happen.
    !------------------------------------------------------------------------------

    module kernelLitter_mod

    use common_modules
    use stateVector_mod
    use background_mod
    use interpolator_mod
    use vintageModules_mod


    type :: kernelLitter_class        !< Litter kernel class
        type(interpolator_class) :: Interpolator !< The interpolator object for the kernel
    contains
    procedure :: initialize => initKernelLitter
    procedure :: DegradationFirstOrder
    procedure :: BeachingHidromod
    end type kernelLitter_class

    public :: kernelLitter_class
    contains

    !---------------------------------------------------------------------------
    !> @author Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Linear degradation kernel.
    !> @param[in] self, sv
    !---------------------------------------------------------------------------
    function DegradationFirstOrder(self, sv)
    class(kernelLitter_class), intent(in) :: self
    type(stateVector_class), intent(inout) :: sv
    real(prec), dimension(size(sv%state,1),size(sv%state,2)) :: DegradationFirstOrder
    integer :: nf, idx
    type(string) :: tag

    DegradationFirstOrder = 0.0
    tag = 'condition'
    nf = Utils%find_str(sv%varName, tag, .true.)
    tag = 'degradation_rate'
    idx = Utils%find_str(sv%varName, tag, .true.)

    DegradationFirstOrder(:,nf) = -sv%state(:,idx)
    where(sv%state(:,nf) < 0.0) sv%active = .false.

    end function DegradationFirstOrder
    
    !---------------------------------------------------------------------------
    !> @author Ricardo Birjukovs Canelas - MARETEC
    !> @brief
    !> Beaching Kernel, uses the already updated state vector and determines if
    !> and how beaching occurs. Affects the state vector and state vector derivative.
    !> @param[in] self, sv, svDt
    !---------------------------------------------------------------------------
    function BeachingHidromod(self, procID, sv, svDt)
    class(kernelLitter_class), intent(inout) :: self
    integer , intent(in) :: procID
    type(stateVector_class), intent(inout) :: sv
    real(prec), dimension(size(sv%state,1),size(sv%state,2)), intent(in) :: svDt
    real(prec), dimension(size(sv%state,1),size(sv%state,2)) :: BeachingHidromod
    integer :: i, idx
    type(string) :: tag
    integer, dimension(6) :: date
    logical, dimension(size(sv%state,1)) :: beached, killPartic
    
    BeachingHidromod = svDt
    beached = .false.
    killPartic = .false.

    if (Globals%Constants%BeachingStopProb /= 0.0) then !beaching is completely turned off if the stopping propability is zero
        tag = 'age'
        idx = Utils%find_str(sv%varName, tag, .true.)
        date = Utils%getDateFromDateTime(Globals%SimTime%CurrDate)
        call vintageModules%runLitterModule(procID,date,sv%state(:,1),sv%state(:,2),sv%state(:,idx), sv%source,sv%id, beached, killPartic)
        where(beached)
            BeachingHidromod(:,1) = 0.0
            BeachingHidromod(:,2) = 0.0
            BeachingHidromod(:,3) = 0.0
            sv%state(:,4) = 0.0
            sv%state(:,5) = 0.0
            sv%state(:,6) = 0.0
        end where
        where(killPartic) sv%active = .false.
        
    end if

    end function BeachingHidromod

    !---------------------------------------------------------------------------
    !> @author Daniel Garaboa Paz - GFNL
    !> @brief
    !> Initializer method adpated from for kernel class. Sets the type of
    !> kernel and the interpolator to evaluate it.
    !---------------------------------------------------------------------------
    subroutine initKernelLitter(self)
    class(kernelLitter_class), intent(inout) :: self
    type(string) :: interpName
    interpName = 'linear'
    call self%Interpolator%initialize(1,interpName)
    end subroutine initKernelLitter

    end module kernelLitter_mod