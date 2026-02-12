#define T int
#define PASTE(a,b) a##b
#define CONCAT(a,b) PASTE(a,b)
#define func_ CONCAT(func_, T)

subroutine func_()
end subroutine func_
