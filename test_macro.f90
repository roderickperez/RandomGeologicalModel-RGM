#define T int
#define CONCAT_HELPER(X, Y) X ## _ ## Y
#define CONCAT(X, Y)        CONCAT_HELPER(X, Y)
#define func_ CONCAT(func, T)

subroutine func_()
end subroutine func_
