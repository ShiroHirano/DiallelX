module m_global_vars
    use m_type_Parameter
    implicit none
    integer, parameter :: kd = 4
    real(kd), parameter :: threshold = 0.0
    integer, parameter :: n_bin = 10000
    integer, parameter :: skip = 10 ! stride for cache blocking technique
    type(Parameter), protected :: prm
end module m_global_vars