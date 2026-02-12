#define CONCAT_HELPER(X, Y) X##_##Y
#define CONCAT(X, Y) CONCAT_HELPER(X, Y)
CONCAT(foo, bar)
