local func = ->{a=_testing_assert_cache_eval:1 b} _testing_assert_cache_eval:(a + b)
local func-twice = ->{a=_testing_assert_cache_eval:1} a + a
local dict = {
	add =  _testing_assert_cache_eval:6 + _testing_assert_cache_eval:7
	arg-twice = func-twice:{}
	plain = _testing_assert_cache_eval: 5
}

add = [ dict.add dict.add dict.add ]
arg-twice = [ dict.arg-twice dict.arg-twice dict.arg-twice ]
plain = [ dict.plain dict.plain dict.plain ]
