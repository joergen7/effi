# effi
###### Erlang foreign function interface.

[![hex.pm](https://img.shields.io/hexpm/v/effi.svg?style=flat-square)](https://hex.pm/packages/effi) [![Build Status](https://travis-ci.org/joergen7/effi.svg?branch=master)](https://travis-ci.org/joergen7/effi)

The Erlang foreign function interface (Effi) is a way to run code in arbitrary foreign languages, e.g., Python or Octave from a native (Erlang) environment.

Herein, the foreign code is executed in an environment that binds a defined set of input variables to values defined from the outside. After completion of the foreign code, a set of output variables is expected to be bound to values.

The foreign code snippet along with input variable bindings is given to effi in the form of a key-value map in a format that can easily be decoded/encoded using the [jsone](https://github.com/sile/jsone) JSON serialization library. Accordingly, effi comes with a command-line application that reads and writes JSON files.

## Supported Languages

- [Bash](https://www.gnu.org/software/bash/)
- [Matlab](https://www.mathworks.com/products/matlab.html)
- [Octave](https://www.gnu.org/software/octave/)
- [Python](https://www.python.org/)

## Usage

### Adding Effi to a Project

Although Effi can be imported also directly from GitHub, we recommend adding a dependency via [hex.pm](https://hex.pm). Here, we show how this can be done using the build tools [rebar3](https://www.rebar3.org) or mix.


#### rebar3

To integrate Effi into a rebar3-managed project change the `deps` entry in your application's `rebar.config` file to include the tuple `{cf_client, "0.1.0"}`.

```erlang
{deps, [{effi, "0.1.5"}]}.
```


#### mix

```elixir
{:effi, "~> 0.1.5"}
```

### Compiling

Having rebar3 available on your system, compile as an Erlang project by entering

    rebar3 compile

If you want to drive the project from the command line please compile it by entering

    rebar3 escriptize

## Starting Effi

Effi can be started in two different ways. It can be started from the command line or it can be driven from an Erlang API.

### Starting from the Command Line

Compiling Effi using `escriptize` creates an Erlang script file `effi` whcih allows starting it via the command line.

To display a help text enter

    ./effi --help

This shows the command line synopsis, which looks like the following:

       ._,,,,  ,,_,=_
        W   `_@__#__     The Erlang Foreign Function Interface (Effi) allows the
       @P+#   F @F @     execution of functions defined in different programming
      _W   y @  # qF     languages (e.g., Bash, Python, or R) by specifying the
      ^^^^^  P qF  `     function's arguments, body and output values.

    Copyright 2015-2018 Jorgen Brandt <joergen.brandt@onlinehome.de>

    Usage: effi [-v] [-h] [-d [<dir>]] [-i <input_file>] [-o <output_file>]

      -v, --version      Show effi version.
      -h, --help         Show command line options.
      -d, --dir          Working directory in which to look for input data and 
                         run the request. [default: .]
      -i, --input_file   Input file holding the effi request (must be 
                         specified).
      -o, --output_file  Output file into which to write the effi reply (must 
                         be specified).


    The input_file and output_file arguments must be specified.

To start Effi from the command line consuming the request file `effi_request.json` and let it produce the reply file `effi_reply.json` enter

    ./effi -i effi_request.json -o effi_reply.json

The format of the request and reply is described below.

### Erlang API

Effi can be driven from an Erlang API by using the function `effi:handle_request/2`. Given an Erlang hash map using atoms as keys and binaries as values bound in the variable `EffiRequest` you can start effi by entering

```erlang
EffiRequest = #{ ... }.
Dir = "./".
effi:handle_request( EffiRequest, Dir ).
```

Effi starts evaluating the request, expecting input data in and writing output data to the working directory given in the variable `Dir`.

## Effi JSON Exchange Format

Effi is driven by task instantiations or applications. Running effi on an application results in a reply. Here, we describe the application format and the reply format.

### Example

Below is an example for an Effi request (an application):

```json
{ "app_id": "1234",
  "lambda": { "lambda_name":  "bowtie2-build",
              "arg_type_lst": [{ "arg_name": "fa",
                                 "arg_type": "File",
                                 "is_list":  false }],
              "ret_type_lst": [{ "arg_name": "idx",
                                 "arg_type": "File",
                                 "is_list":  false }],
              "lang":         "Bash",
              "script":       "bowtie2-build $fa bt2idx\nidx=idx.tar\ntar cf $idx --remove-files bt2idx.*\n" },
  "arg_bind_lst": [{ "arg_name": "fa",
                     "value":    "chr22.fa" }] }
```

The following is an example for an Effi reply:

```json
{ "app_id": "1234",
  "stat":   { "run": { "t_start":  "1523007609917834743",
                       "duration": "30391761645",
                       "node":     "cf_worker@x240" } },
  "result": { "status":       "ok",
              "ret_bind_lst": [{ "arg_name": "idx",
                                 "value":    "idx.tar" }] } }
```

The start time `tstart` is given in nanoseconds from 1970-01-01 and also the the wall-clock running time `duration` is given in nanoseconds. So the example ran `bowtie2-build` for about 30.4 seconds. The `node` field identifies the Erlang node name of the worker instance that ran the task.

### Request Format

The Effi request (application) format is what is consumed. An application `App` has the following form:

    App ::= { "app_id":       S,
              "lambda":       Lambda, 
              "arg_bind_lst": [Bind, ...] }

The application's lambda expression `Lambda` has the following form:

    Lambda ::= { "lambda_name":  S,
                 "arg_type_lst": [TArg, ...],
                 "ret_type_lst": [TArg, ...],
                 "lang":         Lang,
                 "script":       S }

A lambda expression's `arg_type_lst` pair lists specifications for the input parameters while the `ret_type_lst` pair lists specifications for the output parameters of a lambda. An input or output parameter specification `TArg` has the following form:

    TArg ::= { "arg_name": S,
               "arg_type": Type,
               "is_list":  B }

The `arg_type` pair provides the base type of the argument. The base type `Type` has the following form:

    Type ::= "Bool"
           | "Str"
           | "File"


The `lang` pair provides the programming language in which the script is written. The language `Lang` has the following form:

    Lang ::= "Bash"
           | "Matlab"
           | "Octave"
           | "Python"

A lambda expression contains a list of argument bindings `Bind` of the following form:

    Bind ::= { "arg_name": S, "value": S}
           | { "arg_name": S, "value": [S, ...] }

    B ::= true
        | false

    S ::= "..."

### Reply Format

The Effi reply format is what is produced.

    Reply ::= { "app_id": S,
                "result": Result }

    Result ::= { "status": "ok",
                 "stat":   { "t_start": S, "duration": S },
                 "ret_bind_lst": [Bind, ...] }
             | { "status": "error", "stage": "run", "extended_script": S, "output": S }
             | { "status": "error", "stage": "stagein", file_lst: [S, ...] }
             | { "status": "error", "stage": "stageout", file_lst: [S, ...] }






## System Requirements

- [Erlang](https://www.erlang.org) OTP 18.0 or higher
- [Rebar3](https://www.rebar3.org) 3.0.0 or higher

## Resources

- [joergen7/cf_worker](https://github.com/joergen7/cf_worker). A Cuneiform worker implementation.
- [joergen7/cuneiform](https://github.com/joergen7/cuneiform). A functional language for large-scale data analysis.


## Authors

- Jörgen Brandt ([@joergen7](https://github.com/joergen7/)) [joergen.brandt@onlinehome.de](mailto:joergen.brandt@onlinehome.de)

## License

[Apache 2.0](https://www.apache.org/licenses/LICENSE-2.0.html)