# ecl - Expressive Config Language

** Note: ecl is a work in progress. **

ecl is a language designed for creating maintainable configuration, even when the domain is complex. It provides a powerful, yet understandable to let you avoid repetition and express yourself concisely.

## Key Beliefs

- **Obvious**: ecl does what you expect, and be simple to debug when something goes wrong.
- **Expressive**: Configuration gets complex. Instead of hiding from the problem, which often leads to repetition or hacky preprocessing, ecl acknowledges this and provides you the tool you need, while encouraging simplicity.
- **Pure**: ecl evaluates to a simple value. This allows easy verifying your made the changes that you intended. Simply running a diff of the output will show all changes. This also provides referential transparency, this makes it easy to create reusable modules.
- **Standalone**: ecl isn't part of any program or system. It's intended use is to be compiled into a POD format (like JSON or Protocol Buffers) before being used. This gives you an easy introspection point and allows ecl to be used for a wide variety of uses.

## Other features.

- ecl is lazy. This means that you can store multiple related configurations in the same file. You can then easily evaluate them separately.
- Multiple output formats. While you may wish to use something fast like Protocol Buffers in production it is easy to generate other formats that are easier for diffing and debugging.
- Testing. ecl (soon) supports assertions in the configuration to file errors early.

## Plans for new lookup rules and extension rules.

### Simple dict

```
# webapp.ecl
time-zone = "UTC"

database-host = "localhost:1234"
database-name = "webapp"

log-dir = "data/logs"
image-dir = "data/images"

log-level = "warn"
500-debug-info = false # If 500 responses should show debugging information.

sign-cookies = true
require-signed-cookies = true
```

# Complex

```
local webapp = {
	time-zone = err:"Time zone must be set"
	
	option database-host = "localhost"
	option database-port = 1234
	assert "Database port in range" = 0 < database-port < 2**16
	database-addr = "${databse-host}:${database-port}"
	database-name = "webapp"
	
	default base-dir = "data"
	log-dir = "$base-dir/logs"
	image-dir = "$base-dir/images"
	
	log-level = "warn"
	500-debug-info = false # If 500 responses should show debugging information.
	
	sign-cookies = true
	require-signed-cookies = true
}

prod = webapp:{time-zone="UTC"}
dev = webapp:{
	time-zone = "localtime"
	data-dir = "/tmp/webapp-data"
	
	log-level = "info"
	500-debug-info = true
	require-signed-cookies = true
}
```

```
local job = {
	name = panic:"Name required."
	args = {
		service-name = name
	}
}

hello = job:{
	name = "hello"
	args = {
		greating = "Hello"
		name = "World"
		
		assert "Same structure" = service-name == "hello"
		assert "Same lookup" = service-name == "World"
		
		# I like the first one as that is probably what the author meant. It is
		# what you would get if you used a "fully qualified" path (which we
		# don't support) to indicate what property you wanted to reference.
	}
}

greeter = {
	name = "greeter"
	args = job.args:{
		greating = "Hello"
		name = "World"
		
		assert "Closure" = service-name == "hello"
		assert "Same structure" = service-name == "greeter"
		assert "Same lookup" = service-name == "World"
		
		# So this one is interesting, we don't want structural because then
		# you leak all of your local variables. I guess it is like strutural
		# except that you "break" at the interitance boundary. If you hit that
		# you stop looking and go back to lexical. (option 1)
	}
}
```

```
# template.ecl
defaults = {cpu=1 mem=1Gi}

local job = {
	cpu = defaults.cpu
}

# hello.ecl
template = load:./default.ecl
defaults = {cpu=2 mem=4Gi}
hello = template.job:{
	assert closure = cpu == 1
	assert "same structure" = cpu == 2
}
```

