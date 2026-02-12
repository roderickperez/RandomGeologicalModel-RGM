#define T int
#define CONCAT_BASE(X,Y) X##_##Y
#define CONCAT(X,Y) CONCAT_BASE(X,Y)
subroutine CONCAT(func, T)()
end subroutine
