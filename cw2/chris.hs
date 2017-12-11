double x = 2 * x

times 0 f x = f x
times i f x = f (times (i - 1) f x)