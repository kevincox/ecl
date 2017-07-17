local func = ->{a=_testing_assert_cache_eval:1 b} _testing_assert_cache_eval:(a + b)
local dict = {
	plain = _testing_assert_cache_eval: 5
	add =  (_testing_assert_cache_eval:6) + (_testing_assert_cache_eval:7)
	
}

add = [ dict.add dict.add dict.add ]
plain = [ dict.plain dict.plain dict.plain ]
