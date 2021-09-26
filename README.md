# πGPIO
A simple, pure erlang implementation of a module for Raspberry Pi's General Purpose 
Input/Output (GPIO), using the standard Linux kernel interface for user-space, sysfs,
available at /sys/class/gpio/.

This module was created to try robotics on raspberry Pi using erlang and have not tests,
error checking or fancy outputs. It was created for my own use and I'm releasing as it
may be useful for someone.<br>I hope you enjoy!

It was subsequently modified by KingBrewer to be a fully functional OTP application, utilising
gen\_server and supervisor behaviours.

## Requirements
- **Raspbian** GNU/Linux;
- **erlang** 17+, `apt-get install erlang` solve it;
- **root**, to access "/sys/class/gpio/" files. **Note**: alternatively you can change
  the path to gpio files by setting `gpio\_path` environment variable.
- **rebar3**, to speed things up (`wget https://s3.amazonaws.com/rebar3/rebar3 && chmod u+x rebar`)

## Usage
### on projects
- add to dependencies of your rebar.config:
```erlang
{deps, [
    {gpio, {git, "git://github.com/KingBrewer/gpio.git", {branch, master}}},
    ... % other dependencies goes here
]}.
```
- check the settings (`gpio_path`) in `gpio.app.src` file
- then just compile:
```
$ rebar3 compile
```

### Testing without GPIO devices

Project can be run in development mode (for tests) to operate on spoofed
files located in `./test/test_data/sys/class/gpio/`.
Files can be examined locally to ensure the library is working correctly.

**Note**: In test mode only pins `1` and `2` are supported.

```erlang-repl
$ rebar3 shell --config config/test.config
Erlang/OTP 17 [erts-6.2] [source] [async-threads:10] [kernel-poll:false]

Eshell V6.2  (abort with ^G)
1> ===> Booted gpio
1> L0 = gpio:init(1, out).
1
2> gpio:write(L0, 0).
ok
3> L1 = gpio:init(2, out).
2
4> gpio:write(L1, 1).
ok
5> gpio:stop().
ok
```

```bash
$ cat ./test/test_data/sys/class/gpio/gpio1/value
0

$ cat ./test/test_data/sys/class/gpio/gpio2/value
1
```

### on REPL

```erlang-repl
$ rebar3 shell
Erlang/OTP 17 [erts-6.2] [source] [async-threads:10] [kernel-poll:false]

Eshell V6.2  (abort with ^G)
1> ===> Booted gpio
1> L0 = gpio:init(21, out).
21
2> L1 = gpio:init(23, in).
23
3> gpio:write(L0, 1).
ok
4> gpio:read(L0).
"1"
5> gpio:stop().
ok
```

## Documentation
- **start()** Starts the gpio application with default settings.
- **stop()** Stops the entire application and .
- **init(Pin, Direction)** Initialize a **pin** as *output* or *input*, according to **Direction**(an atom). It returns a *Reference* to *Pin*.
- **init(Pin)** A shortcut to initialize *pin* as *output*. It, also, returns a *Reference* to *Pin*.
- **stop(Ref)** Stop using and release th **pin** referenced as **Ref**. It returns an atom, **ok**.
- **write(Ref, Val)** Writes **Val**, 1 or 0, into the **pin** initialized and referenced as **Ref**.
- **read(Ref)** Reads a value(1 or 0) from **pin** referenced as **Ref** and returns it.

For the official erlang-style documentation, run:
```
$ erl -noshell -run edoc_run application 'gpio' '"."' '[]'
```

## Tested on
- Erlang/OTP 17 erts-6.2 in GNU/Linux Raspian 8.0 (jessie) kernel 4.1.19+ armv61 on Raspberry B+
- Erlang R15B01 erts-5.9.1 in GNU/Linux Raspian 7 (wheezy) kernel 4.1.13+ armv6l on Raspberry B.

## License
[MIT License](LICENSE.md) © 2015-2016 Paolo Oliveira.
