# assumes included at a certain position in `AtState.rb`
# TODO: fix that

# NOTE: prefer `inst.cast_list` over `force_list`

def default_sentinel(*values, sentinel: AtFunctionCatalog::NOT_PROVIDED, &final)
    values.each { |e|
        return e if e != sentinel
    }
    return final[] if final
    sentinel
end
module AtFunctionCatalog
    NOT_PROVIDED = :not_provided

    # TODO: remove @@configurable and @@held_arguments in favor of `held`
    # and `configurable` functions
    # functions which can receive key things
    @@configurable = [
        "Bisect",
        "Chop",
        "Configure",
        "Print",
        "Option",
        "Safely",
        "Series",
        "SeriesIf",
    ]
    # functions whose arguments are not evaluated at once
    # (true = not evaluated, false = evaluated (normal))
    HOLD_ALL = Hash.new(true)
    @@held_arguments = {
        "->" => [true, false],
        "→" => [true, false],
        "If" => [false, true, true],
        "While" => [true, true],
        "DoWhile" => [true, true],
        "ForEach" => [false, true],
        "Modify" => [false, true],
        ":=" => [true, true],
        ".=" => [true, true],
        "." => [false, true],
        "DoSafe" => [true],
        "TryCatch" => [true, true],

        "and" => [true, true],
        "nand" => [true, true],
        "else" => [true, true],
        "or" => [true, true],
        "nor" => [true, true],
        "not" => [true, true],
    }

    # All builtins
    @@functions = {
        ###########################
        #### GENERIC FUNCTIONS ####
        ###########################
        ## Contains:
        ## - control flow
        ## - memory manipulation
        ## - i/o
        ## - functions that are necessary

        #<<
        # Reads all input from the instance's source input.
        # @return string
        # @genre IO
        #>>
        "AllInput" => lambda { |inst|
            inst.in.read
        },

        #<<
        # Returns the <code>n</code>th argument given to the program.
        # @return string
        # @genre IO
        # @type n number
        #>>
        "Arg" => lambda { |inst, n=0|
            ARGV[n + 1]
        },
        #<<
        # Undefines <code>name</code> from the global scope. Returns that variable's value, or <code>nil</code> if the variable was undefined.
        # @return (*)
        # @genre scope
        # @type name string
        #>>
        "Clear" => lambda { |inst, name|
            inst.clear name
        },
        #<<
        # Clears the console's screen.
        # @return nil
        # @genre IO
        #>>
        "ClearScreen" => lambda { |inst|
            system "cls" or system "clear"
        },
        #<<
        # Undefines <code>name</code> from the local scope. Returns that variable's value, or <code>nil</code> if the variable was undefined.
        # @return (*)
        # @genre scope
        # @type name string
        #>>
        "ClearLocal" => lambda { |inst, name|
            inst.clear_local name
        },
        #<<
        # Returns a function which calls <code>func</code> configured with <code>opts</code>.
        # @type func fn
        # @return fn
        # @example print_weird := Configure[
        # @example     Print,
        # @example     joiner->", ",
        # @example     before->"{",
        # @example     after->"}\n"
        # @example ]
        # @example
        # @example print_weird[3, 4, 5]
        # @example ?? {3, 4, 5}
        # @genre functional
        #>>
        "Configure" => lambda { |inst, func, **opts|
            lambda { |inst, *args|
                func[inst, *args, **opts]
            }
        },
        #<<
        # Defines <code>name</code> in the global scope as <code>value</code>. Returns <code>value</code>.
        # @return (*)
        # @genre scope
        # @type name string
        # @type value (*)
        # @example Define[$a, "This is variable a!"]
        # @example Print[a]
        # @example ?? This is variable a!
        #>>
        "Define" => lambda { |inst, name, value|
            inst.define name, value
        },
        #<<
        # Returns the value of the variable whose name is <code>name</code>.
        # @return (*)
        # @type name string
        # @genre scope
        # @example a := 323
        # @example Print[Retrieve[$a]]
        # @example ?? 323
        #>>
        "Retrieve" => lambda { |inst, name|
            inst.get_variable name
        },
        #<<
        # Displays <code>ent</code> in a human-reversible format. Primarily, prints the elements of arrays (whose dimension is at least 2) on separate lines.
        # @return (*)
        # @type ent (*)
        # @genre IO
        # @example Display[ [1:3, 4:6, 9:11] ]
        # @example ??  1  2  3
        # @example ??  4  5  6
        # @example ??  9 10 11
        # @example Display[ "Hello, World!" ]
        # @example ?? "Hello, World!"
        #>>
        "Display" => lambda { |inst, ent|
            display ent
        },
        #<<
        # Evaluates <code>str</code> in a new scope. Returns the last expression evaluated.
        # @return (*)
        # @type str string
        # @genre meta
        #>>
        "Eval" => lambda { |inst, str|
            AtState.new(str).run.last
        },
        #<<
        # Evaluates <code>str</code> in a the current scope. Returns the last expression evaluated.
        # @return (*)
        # @type str string
        # @genre meta
        #>>
        "EvalHere" => lambda { |inst, str, blanks=[]|
            ast(str).map { |tree|
                inst.evaluate_node tree, blanks
            }.last
        },
        #<<
        # Terminates the program with exit code <code>0</code>. Flushes STDOUT.
        # @optional code
        # @param code the code to exit with.
        # @type code number
        # @return nil
        # @genre IO
        #>>
        "Exit" => lambda { |inst, code=0|
            STDOUT.flush
            exit! code
        },
        #<<
        # Creates a hash.
        # @return Hash[string -> (*)]
        # @type opts ConfigureValue
        # @param opts A series of value pairs <code>a -> v</code>.
        # @genre data
        #>>
        "Hash" => lambda { |inst, *opts|
            res = {}
            # p opts
            opts.each { |k, v|
                res[k] = v
            }
            res
        },
        # "Hold" => lambda { |inst, fn|
        #     if AtState.func_like? fn
        #         fn ;
        #     end
        # },
        #<<
        # Defines <code>name</code> in the local scope as <code>value</code>. Returns <code>value</code>.
        # @return (*)
        # @genre scope
        # @type name string
        # @type value (*)
        #>>
        "Local" => lambda { |inst, name, value|
            inst.define_local name, value
        },
        #<<
        # Modifies <code>head</code> according to <code>body</code>.
        # @type head string
        # @param body an expression using the first abstract variable <code>_</code> to represent the original value.
        # @type body expression
        # @return (*)
        # @genre scope
        # @example a := 3
        # @example Modify[$a, _ * 2 + 1]
        # @example Print[a]
        # @example ?? 7
        #>>
        "Modify" => lambda { |inst, head, body|
            init = inst.get_variable head
            result = inst.evaluate_node body, [init]
            inst.define head, result
        },
        #<<
        # Imports libraries. Returns list of libaries' names included.
        # @type libs string
        # @param libs list of strings corresponding to library names.
        # @genre meta
        # @return [string]
        #>>
        "Needs" => lambda { |inst, *libs|
            libs.each { |lib|
                inst.load_lib lib
            }
        },
        #<<
        # Prints <code>prompt</code> and waits for character input found in <code>opts</code>.
        # @type prompt string
        # @return string
        # @genre IO
        # @example input := Option["Press something!",
        # @example     s -> "stop this program",
        # @example     i -> "print info",
        # @example     h -> "print hello world"
        # @example ]
        # @example ?? outputs "Press something!"
        # @example ?? user presses the `i` key
        # @example Print["Received:", Repr[input]]
        # @example ?? Received: "i"
        #>>
        "Option" => lambda { |inst, prompt, **opts|
            read_option prompt, opts
        },
        #<<
        # Returns the successor of <code>ent</code>.
        # @type ent (*)
        # @return (*)
        # @genre data
        #>>
        "Pred" => lambda { |inst, ent|
            ent.pred
        },
        #<<
        # Prints each argument of <code>args</code>, separated by spaces.
        # @type args (*)
        # @return [(*)]
        # @option after String printed after everything else. Default: <code>"\n"</code>.
        # @option before String printed before everything else. Default: <code>""</code>.
        # @option joiner The string which joins <code>args</code>. Default: <code>" "</code>.
        # @genre IO
        #>>
        "Print" => lambda { |inst, *args, **opts|
            joiner = opts[:joiner] || " "
            inst.out.print opts[:before] || ""
            inst.out.print args.map { |e|
                inst.cast_string e
            }.join(joiner)
            inst.out.print opts[:after] || "\n"
            args
        },
        #<<
        # Prompts for a line of input. Returns string without trailing newline.
        # @return string
        # @type prompt string
        # @param prompt String to display before input text. Nothing if omitted.
        # @optional prompt
        # @genre IO
        #>>
        "Prompt" => lambda { |inst, prompt=nil|
            prompt_input prompt, inst.in
        },
        #<<
        # Reads a line of input, to include any trailing newline.
        # @return string
        # @genre IO
        #>>
        "ReadLine" => lambda { |inst|
            inst.in.gets
        },
        #<<
        # Iterates <code>func</code> over lines of stdin received by <code>Prompt[...args]</code>.
        # Returns a list of these lines modified by <code>func</code>.
        # @return [string]
        # @type args (*)
        # @type func fn[string -> string]
        # @param func A function that receives lines of input and returns a string.
        # @genre IO
        #>>
        "ReadLineLoop" => lambda { |inst, *args, func|
            lines = []
            loop {
                line = @@functions["Prompt"][inst, *args]
                break if line.nil?
                lines << func[inst, line]
            }
            lines
        },
        #<<
        # Reads a character from the input.
        # @return string
        # @genre IO
        #>>
        "ReadChar" => lambda { |inst|
            inst.in.getc
        },
        #<<
        # Reads an integer from the input.
        # @return number
        # @genre IO
        #>>
        "ReadInt" => lambda { |inst|
            inst.in.gets.chomp.to_i
        },
        #<<
        # Reads a float from the input.
        # @return number
        # @genre IO
        #>>
        "ReadFloat" => lambda { |inst|
            inst.in.gets.chomp.to_f
        },
        #<<
        # Reads a number from the input.
        # @return number
        # @genre IO
        #>>
        "ReadNumber" => lambda { |inst|
            force_number inst.in.gets.chomp
        },
        #<<
        # Updates values of global abstracts, respective to <code>args</code>. <code>_1</code> would become <code>args[0]</code>, and <code>_<em>n</em></code> would become <code>args[n-1]</code>.
        # @type args (*)
        # @return [(*)]
        # @genre meta
        #>>
        "Save" => lambda { |inst, *args|
            inst.saved = args
        },
        #<<
        # Reads all of STDIN.
        # @return string
        # @genre IO
        #>>
        "Stdin" => lambda { |inst|
            STDIN.read
        },
        #<<
        # Returns the successor of <code>ent</code>.
        # @type ent (*)
        # @return (*)
        # @genre data
        #>>
        "Succ" => lambda { |inst, ent|
            ent.succ
        },
        #<<
        # Writes <code>args</code>, joined together, to STDOUT.
        # @type args (*)
        # @genre IO
        # @return nil
        #>>
        "Stdout" => lambda { |inst, *args|
            print args.flatten.join
        },
        #<<
        # Returns an array of the arguments.
        # @type args (*)
        # @return [(*)]
        # @genre data
        #>>
        "V" => lambda { |inst, *args|
            args
        },

        ##---------##
        ## File IO ##
        ##---------##
        #<<
        # Returns the contents of file <code>name</code>, or <code>nil</code> if the file does not exist.
        # @type name string
        # @return string
        # @genre IO/files
        # @option encoding The encoding that the target file is in.
        #>>
        "FileRead" => lambda { |inst, name, **opts|
            opts[:encoding] ||= "UTF-8"
            File.read(name.strip, encoding: opts[:encoding]) rescue nil
        },
        #<<
        # Writes <code>content</code> to file <code>name</code>. Returns the number of bytes written.
        # @type name string
        # @type content string
        # @return number
        # @genre IO/files
        # @option encoding The encoding that the target file is in.
        #>>
        "FileWrite" => lambda { |inst, name, content, **opts|
            opts[:encoding] ||= "UTF-8"
            File.write(name.strip, content, encoding: opts[:encoding])
        },
        #<<
        # Returns <code>true</code> if <code>name</code> represents a valid file, <code>false</code> otherwise.
        # @type name string
        # @return bool
        # @genre IO/files
        #>>
        "FileExists" => lambda { |inst, name|
            File.exists? name.strip
        },
        "Wait" => lambda { |inst, n|
            sleep n
        },
        "Cls" => lambda { |inst|
            cls
        },

        #################
        #### CLASSES ####
        #################
        #<<
        # Creates an anonymous class.
        # @type body fn
        # @param body Any local definition made within constitutes a method or instance variable decleration.
        # @return class
        # @genre class
        #>>
        "Class" => lambda { |inst, body, parent=nil|
            AtClass.new inst, body, parent
        },
        #<<
        # Creates a named class. Returns a function which acts similarly to <a href="#Class"><code>Class</code></a>.
        # @type name string
        # @return fn
        # @genre class
        #>>
        "ClassNamed" => held(true) { |inst, name, parent=nil|
            #p name
            lambda { |inst, body|
                inst.define name.raw, AtClass.new(inst, body, parent, name: name.raw)
            }
        },
        #<<
        # Instantiates a class with parameters <code>args</code>.
        # @type ac class
        # @type args (*)
        # @return class[]
        # @genre class
        #>>
        "New" => lambda { |inst, ac, *args|
            ac.create(*args)
        },

        #############################
        #### UNIVERSAL FUNCTIONS ####
        #############################
        #<<
        # Identity function; returns its argument.
        # @return (*)
        # @type a (*)
        # @genre functional
        #>>
        "Id" => lambda { |inst, a|
            a
        },
        #<<
        # Pops a member from the end of <code>list</code>, modifying it. Returns that element.
        # @type list [(*)]
        # @return (*)
        # @genre list
        #>>
        "Pop" => lambda { |inst, list|
            list.pop
        },
        #<<
        # Pops a member from the start of <code>list</code>, modifying it. Returns that element.
        # @type list [(*)]
        # @return (*)
        # @genre list
        #>>
        "Shift" => lambda { |inst, list|
            list.shift
        },
        #<<
        # Pushes <code>args</code> to the end of <code>list</code>, modifying it. Returns <code>list</code>.
        # @type list [(*)]
        # @type args (*)
        # @return [(*)]
        # @genre list
        #>>
        "Push" => lambda { |inst, list, *args|
            list.push *args
        },
        #<<
        # Pushes <code>args</code> to the start of <code>list</code>, modifying it. Returns <code>list</code>.
        # @type list [(*)]
        # @type args (*)
        # @return [(*)]
        # @genre list
        #>>
        "Unshift" => lambda { |inst, list, *args|
            list.unshift *args
        },
        #<<
        # Determines if <code>ent</code> is palindromic, that is, if it is itself reversed.
        # @type ent [(*)]|string
        # @return bool
        # @genre list/logic
        #>>
        "Palindromic" => lambda { |inst, ent|
            reverse(ent) == ent
        },
        #<<
        # Reverses the elements of <code>ent</code>.
        # @return (*)
        # @type ent (*)
        # @genre list
        #>>
        "Reverse" => lambda { |inst, ent|
            reverse ent
        },

        ##########################
        #### HYBRID FUNCTIONS ####
        ##########################
        #<<
        # Obtains all non-negative values under <code>max</code> by repeating <code>f</code>.
        # @return [number]
        # @type f fn[number]
        # @type max number
        # @type start number
        # @optional start
        # @genre functional
        # @option include determines whether or not to include <code>max</code> as an upperbound. Default: <code>false</code>
        # @example Print[Series[Prime, 13]]
        # @example ?? [2, 3, 5, 7, 11]
        # @example Print[Series[Prime, 13, include->true]]
        # @example ?? [2, 3, 5, 7, 11, 13]
        #>>
        "Series" => lambda { |inst, f, max, start=0, **config|
            i = start
            collect = []
            loop {
                value = f[inst, i]
                unless value.nil?
                    break if config[:include] ? value > max : value >= max
                    collect.push value
                end
                i = @@functions["Succ"][inst, i]
            }
            collect
        },
        #<<
        # Obtains all non-negative values under <code>max</code> which satisfy <code>cond</code> by repeating <code>f</code>.
        # @return [number]
        # @type f fn[n]
        # @type cond fn[n->b]
        # @type max number
        # @type start number
        # @optional start
        # @genre functional
        # @option include determines whether or not to include <code>max</code> as an upperbound. Default: <code>false</code>
        # @example Print[SeriesIf[Prime, Odd, 13]]
        # @example ?? [3, 5, 7, 11]
        # @example Print[SeriesIf[Prime, Odd, 13, include->true]]
        # @example ?? [3, 5, 7, 11, 13]
        #>>
        "SeriesIf" => lambda { |inst, f, cond, max, start=0, **config|
            i = start
            collect = []
            loop {
                value = f[inst, i]
                unless value.nil?
                    break if config[:include] ? value > max : value >= max
                    collect.push value if cond[inst, value]
                end
                i = @@functions["Succ"][inst, i]
            }
            collect
        },
        #<<
        # Returns elements from <code>list</code> until before the first element which does not satisfy <code>cond</code>.
        # @type cond fn
        # @type list [(*)]
        # @return list
        # @reforms
        # @genre functional
        # @example Display[TakeWhile[{_ /= sp}, "Hello, World!"]]
        # @example ?? "Hello,"
        # @example Print[TakeWhile[Even, [2, 4, 6, 1, 2, 3]]]
        # @example ?? [2, 4, 6]
        # @example Print[TakeWhile[IsPrime, 357923]]
        # @example ?? 357
        #>>
        "TakeWhile" => lambda { |inst, cond, list|
            collect = []

            inst.cast_list(list).each { |el|
                break unless cond[inst, el]
                collect << el
            }

            reform_list collect, list
        },
        #<<
        # Starting at <code>n = start</code>, increments <code>n</code> until
        # <code>cond[f[n]]</code> is truthy. Returns <code>f[n]</code>. Note: this ignores <code>nil</code> values returned.
        # @type f fn
        # @type cond fn
        # @type start number
        # @optional start
        # @param f A function that maps a number to a domain, such as <code>Square</code> or <code>Prime</code>.
        # @param cond A function which, given a possible result of <code>f</code>, returns a truthy or falsey value.
        # @param start The starting point for testing numbers.
        # @return (*)
        # @genre functional
        #>>
        "GenerateFirst" => lambda { |inst, f, cond, start=0|
            res = nil
            n = start
            loop {
                res = f[inst, n]
                unless res.nil?
                    break if AtState.truthy? cond[inst, res]
                end
                n = @@functions["Succ"][inst, n]
            }
            res
        },
        #<<
        # Similar to <a href="#GenerateFirst"><code>GenerateFirst</code></a>, returns the first <code>size</code> values of <code>f</code> satisfying <code>cond</code>
        # @type size number
        # @type f fn
        # @type cond fn
        # @type start number
        # @optional start
        # @param f A function that maps a number to a domain, such as <code>Square</code> or <code>Prime</code>.
        # @param cond A function which, given a possible result of <code>f</code>, returns a truthy or falsey value.
        # @param start The starting point for testing numbers.
        # @return (*)
        # @genre functional
        #>>
        "GenerateN" => lambda { |inst, f, cond, size, start=0|
            res = nil
            collect = []
            n = start
            until collect.size >= size
                res = f[inst, n]
                if !res.nil? && AtState.truthy?(cond[inst, res])
                    collect << res
                end
                n = @@functions["Succ"][inst, n]
            end
            collect
        },

        ###########################
        #### NUMERIC FUNCTIONS ####
        ###########################
        #<<
        # Calculates the absolute value of <code>n</code>.
        # @return number
        # @type n number
        # @genre numeric
        #>>
        "Abs" => vectorize_monad { |inst, n|
            n.abs
        },
        #<<
        # Adds each of <code>args</code> together.
        # @return number
        # @type args number
        # @genre numeric
        #>>
        "Add" => lambda { |inst, *args|
            @@functions["Sum"][inst, args]
        },
        #<<
        # Converts <code>n</code> to base <code>2</code>.
        # @type n number
        # @return number
        # @genre numeric/bases
        #>>
        "Bin" => vectorize_monad { |inst, n|
            @@functions["ToBase"][inst, n, 2]
        },
        #<<
        # Returns <code>n</code> rounded up to the nearest integer.
        # @return number
        # @type n number
        # @param r the precision to round. No precision if omitted.
        # @type r number
        # @optional r
        # @genre numeric
        #>>
        "Ceiling" => vectorize_dyad { |inst, n, r=nil|
            if r.nil?
                n.ceil
            else
                n.ceil(r)
            end
        },
        #<<
        # Converts <code>arg</code> to characters.
        # @type arg number|[number]
        # @return string
        # @paramtype number arg Converts <code>arg</code> to a character.
        # @paramtype [number] arg Converts <code>arg</code> to a string of char codes.
        # @genre conversion
        #>>
        "Char" => lambda { |inst, arg|
            if arg.is_a? Array
                arg.map { |e| e.chr Encoding::UTF_8 }.join
            else
                arg.chr Encoding::UTF_8 
            end
        },
        #<<
        # Produces the Collatz sequence of <code>n</code>.
        # @type n number
        # @return [number]
        # @genre numeric/series
        #>>
        "Collatz" => vectorize_monad { |inst, n|
            collatz n
        },
        #<<
        # Returns the number of steps it takes for <code>n</code> to reach <code>1</code> according to the Collatz transformation.
        # @type n number
        # @return number
        # @genre numeric/series
        #>>
        "CollatzSize" => vectorize_monad { |inst, n|
            collatz(n).size - 1
        },
        #<<
        # Doubles <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Double" => vectorize_monad { |inst, n|
            @@operators["*"][inst, n, 2]
        },
        #<<
        # Produces the digits of <code>n</code>.
        # @type n number
        # @return [number]
        # @genre numeric
        #>>
        "Digits" => vectorize_monad { |list, n|
            n.digits.reverse
        },
        #<<
        # Divides each number in <code>args</code> by the next. That is, folding division over <code>args</code>
        # @type args number
        # @return number
        # @genre numeric
        #>>
        "Divide" => lambda { |inst, *args|
            args[0] = args[0].to_f
            args.inject(:/)
        },
        "DivMod" => vectorize_dyad { |inst, d, m|
            d.divmod m
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>e</mi><mi>n</mi></msup></math>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Exp" => lambda { |inst, n|
            CMath::exp n
        },
        #<<
        # Returns the <code>n</code>th number in the Fibonacci sequence, starting with <code>f<sub>0</sub> = 0</code> and <code>f<sub>1</sub> = 1</code>.
        # @type n number
        # @return number
        # @genre numeric/series
        #>>
        "Fibonacci" => lambda { |inst, n|
            nth_fibonacci(n)
        },
        #<<
        # Returns <code>n</code> rounded down to the nearest integer.
        # @return number
        # @type n number
        # @param r the precision to round. No precision if omitted.
        # @type r number
        # @optional r
        # @genre numeric
        #>>
        "Floor" => vectorize_dyad { |inst, n, r=nil|
            if r.nil?
                n.floor
            else
                n.floor(r)
            end
        },
        #<<
        # Converts <code>num</code> from base <code>base</code>
        # to base <code>10</code>.
        # @return number
        # @type num [number]
        # @param num an array of digits representing the number in base <code>base</code>.
        # @type base number
        # @param base a number greater than <code>0</code>, representing the source base of the numeric array.
        # @genre numeric/bases
        #>>
        "FromBase" => vectorize_dyad(RIGHT) { |inst, num, base|
            from_base num, base
        },
        #<<
        # Takes the Greatest Common Divisor of the atoms of <code>args</code>.
        # @type args number
        # @param args List of numbers, which can have nested elements.
        # @return number
        # @genre numeric
        #>>
        "GCD" => lambda { |inst, *args|
            gcd args.flatten
        },
        #<<
        # Converts <code>n</code> to base <code>16</code>.
        # @type n number
        # @return number
        # @genre numeric/bases
        #>>
        "Hex" => vectorize_monad { |inst, n|
            @@functions["ToBase"][inst, n, 16]
        },
        #<<
        # Takes half of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Halve" => vectorize_monad { |inst, n|
            @@operators["/"][inst, n, 2]
        },
        #<<
        # Takes the Least Common Multiple of the atoms of <code>args</code>.
        # @type args number
        # @param args List of numbers, which can have nested elements.
        # @return number
        # @genre numeric
        #>>
        "LCM" => lambda { |inst, *args|
            lcm args.flatten
        },
        #<<
        # Calculates the discrete logarithm of <code>n</code> mod <code>base</code>.
        # @type n number
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "IntLog" => vectorize_dyad { |inst, n, base|
            discrete_log n, base
        },
        #<<
        # Returns <code>true</code> is <code>n</code> is a
        # <a href="#Rational"><code>Rational</code></a>, <code>false</code>
        # otherwise.
        # @type x (*)
        # @return bool
        # @genre numeric/logic
        #>>
        "IsRational" => lambda { |inst, x|
            Rational === x
        },
        #<<
        # Takes the base-<code>10</code> logarithm of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Log" => vectorize_monad { |inst, n|
            CMath::log10 n
        },
        #<<
        # Takes the base-<code>2</code> logarithm of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Log2" => vectorize_monad { |inst, n|
            CMath::log2 n
        },
        #<<
        # Takes the natural logarithm of <code>n</code>. Note that <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>ln</mi><mo>(</mo><mi>n</mi><mo>)</mo><mo>=</mo><msub><mi>log</mi><mi>e</mi></msub><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @return number
        # @type n number
        # @genre numeric
        #>>
        "Ln" => vectorize_monad { |inst, n|
            CMath::log n
        },
        #<<
        # Takes the base-<code>b</code> logarithm of <code>n</code>.
        # @return number
        # @type n number
        # @genre numeric
        #>>
        "LogBase" => vectorize_dyad { |inst, n, b|
            force_number CMath::log n, b
        },
        #<<
        # Takes the product of the elements of <code>args</code>.
        # @return number
        # @type args number
        # @genre numeric
        #>>
        "Multiply" => lambda { |inst, *args|
            @@functions["Prod"][inst, args]
        },
        #<<
        # Converts <code>ent</code> to an integer.
        # @type ent [number]|string|number|(*)
        # @return number
        # @genre conversion
        # @paramtype [number] ent Converts <code>ent</code> to base <code>10</code>.
        # @paramtype string ent Parses <code>ent</code> as a base <code>10</code> integer.
        # @paramtype number ent Converts <code>ent</code> to an integer if it represents one.
        # @paramtype (*) ent Attempts to cast <code>ent</code> to an integer.
        #>>
        "N" => lambda { |inst, ent|
            force_number ent
        },
        #<<
        # Converts <code>n</code> to base <code>8</code>.
        # @type n number
        # @return number
        # @genre numeric/bases
        #>>
        "Oct" => vectorize_monad { |inst, n|
            @@functions["ToBase"][inst, n, 8]
        },
        #<<
        # Returns <code>[a - b, a + b]</code>.
        # @return [number]
        # @type a number
        # @type b number
        # @genre numeric
        #>>
        "PlusMinus" => vectorize_dyad { |inst, a, b|
            [@@operators["+"][inst, a, b], @@operators["-"][inst, a, b]]
        },
        #<<
        # Obtains the <code>n</code>th <code>order</code>-agonal number.
        # @return number
        # @type n number
        # @type order number
        # @genre numeric/series
        # @example (* The triangular numbers (default) *)
        # @example Print[Polygonal[0:5, 3]]
        # @example ?? [0, 1, 3, 6, 10, 15]
        # @example Print[Polygonal[0:5]]
        # @example ?? [0, 1, 3, 6, 10, 15]
        # @example
        # @example (* The square numbers *)
        # @example Print[Polygonal[0:5, 4]]
        # @example ?? [0, 1, 4, 9, 16, 25]
        #>>
        "Polygonal" => vectorize_dyad { |inst, n, order=3|
            gonal n, order
        },
        #<<
        # Returns the <code>n</code>th enumeration of the Pythagorean triples.
        # @return [number]
        # @type n number
        # @genre numeric/series
        # @example Print => Pythagorean[0:10]
        # @example ?? [3, 4, 5]
        # @example ?? [8, 6, 10]
        # @example ?? [5, 12, 13]
        # @example ?? [15, 8, 17]
        # @example ?? [12, 16, 20]
        # @example ?? [7, 24, 25]
        # @example ?? [24, 10, 26]
        # @example ?? [21, 20, 29]
        # @example ?? [16, 30, 34]
        # @example ?? [9, 40, 41]
        # @example ?? [35, 12, 37]
        #>>
        "Pythagorean" => vectorize_monad { |inst, n|
            pythagorean n
        },
        #<<
        # Returns a pseudo-random number. By default, returns a random float between <code>0</code> (inclusive) and <code>1</code> (exclusive).
        # @param n If <code>m</code> is omitted, returns a random integer between <code>0</code> (inclusive) and <code>n</code> (exclusive).
        # @param m When provided, returns a random integer between <code>n</code> and <code>m</code>, inclusive.
        # @optional n
        # @optional m
        # @type n number
        # @type m number
        # @return number
        # @genre numeric
        #>>
        "Random" => vectorize_dyad { |inst, n=nil, m=nil|
            random(n, m)
        },
        #<<
        # Returns a fraction representing <code>a / b</code>.
        # @return rational
        # @type a number
        # @type b number
        # @genre numeric
        #>>
        "Rational" => vectorize_dyad { |inst, a, b|
            Rational(a, b)
        },
        #<<
        # Returns <code>n</code> rounded half-up to the nearest integer.
        # @return number
        # @type n number
        # @param r the precision to round. No precision if omitted.
        # @type r number
        # @optional r
        # @genre numeric
        #>>
        "Round" => vectorize_dyad { |inst, n, r=nil|
            if r.nil?
                n.round
            else
                n.round(r)
            end
        },
        #<<
        # Returns the sign of <code>n</code>; Returns <code>-1</code> if <code>n</code> is negative, <code>1</code> if it is positive, and <code>0</code> otherwise.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Sign" => vectorize_monad { |inst, n|
            sign n
        },
        #<<
        # Returns the numerator of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric/rational
        #>>
        "Numerator" => vectorize_monad { |inst, n|
            Rational(n).numerator
        },
        #<<
        # Returns the numerator of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric/rational
        #>>
        "Denominator" => vectorize_monad { |inst, n|
            Rational(n).denominator
        },
        #<<
        # Returns the square root of <code>n</code>. The result is imaginary if <code>n</code> is negative.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Sqrt" => vectorize_monad { |inst, n|
            CMath::sqrt n
        },
        #<<
        # Returns the cube root of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Cbrt" => vectorize_monad { |inst, n|
            CMath::cbrt n
        },
        #<<
        # Returns the <code>b</code>th root of <code>n</code>.
        # @type n number
        # @type b number
        # @return number
        # @genre numeric
        #>>
        "Root" => vectorize_dyad { |inst, b, n|
            require 'bigdecimal'
            force_number n ** (BigDecimal.new(1) / b)
        },
        #<<
        # Returns the square of <code>n</code>.
        # @type n number
        # @return number
        # @genre numeric
        #>>
        "Square" => vectorize_monad { |inst, n|
            @@operators["*"][inst, n, n]
        },
        #<<
        # Subtracts each number in <code>args</code> by the next. That is, folding subtraction over <code>args</code>.
        # @type args number
        # @return number
        # @genre numeric
        #>>
        "Subtract" => lambda { |inst, *args|
            args.inject(:-)
        },
        #<<
        # Converts <code>num</code> to base <code>base</code>.
        # @type num number
        # @type base number
        # @return [number]
        # @genre numeric/bases
        #>>
        "ToBase" => vectorize_dyad { |inst, num, base|
            to_base num, base
        },
        #<<
        # Returns the <code>n</code>th Triangular number.
        # @type n number
        # @return number
        # @example Print[Triangular[0:5]]
        # @example Print[Polygonal[0:5, 3]]
        # @example
        # @example ?? [0, 1, 3, 6, 10, 15]
        # @genre numeric/series
        #>>
        "Triangular" => vectorize_monad { |inst, n|
            gonal n, 3
        },
        #<<
        # Converts from base <code>2</code> (binary) to base <code>10</code>.
        # @genre numeric/bases
        # @return number
        # @type n [number]
        #>>
        "UnBin" => lambda { |inst, n|
            @@functions["FromBase"][inst, n, 2]
        },
        #<<
        # Converts from base <code>16</code> (hexadecimal) to base <code>10</code>.
        # @genre numeric/bases
        # @return number
        # @type n [number]
        #>>
        "UnHex" => lambda { |inst, n|
            @@functions["FromBase"][inst, n, 16]
        },
        #<<
        # Converts from base <code>8</code> (octal) to base <code>10</code>.
        # @genre numeric/bases
        # @return number
        # @type n [number]
        #>>
        "UnOct" => lambda { |inst, n|
            @@functions["FromBase"][inst, n, 8]
        },

        ##------------------##
        ## Binary Functions ##
        ##------------------##
        "And" => lambda { |inst, a, b|
            AtState.truthy?(a) && AtState.truthy?(b)
        },
        "Nand" => lambda { |inst, a, b|
            !(AtState.truthy?(a) && AtState.truthy?(b))
        },
        "Or" => lambda { |inst, a, b|
            AtState.truthy?(a) || AtState.truthy?(b)
        },
        "Nor" => lambda { |inst, a, b|
            !(AtState.truthy?(a) || AtState.truthy?(b))
        },
        "Xor" => lambda { |inst, a, b|
            AtState.truthy?(a) == AtState.truthy?(b)
        },
        "Xnor" => lambda { |inst, a, b|
            AtState.truthy?(a) != AtState.truthy?(b)
        },
        "BitAnd" => lambda { |inst, a, b|
            a & b
        },
        "BitOr" => lambda { |inst, a, b|
            a | b
        },
        "BitXor" => lambda { |inst, a, b|
            a ^ b
        },
        "BitNot" => lambda { |inst, a|
            ~a
        },
        "BitNand" => lambda { |inst, a, b|
            ~(a & b)
        },
        "BitNor" => lambda { |inst, a, b|
            ~(a | b)
        },
        "BitXnor" => lambda { |inst, a, b|
            ~(a ^ b)
        },

        ##-------------------------##
        ## Trigonometric Functions ##
        ##-------------------------##
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>cos</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcCos" => vectorize_monad { |inst, n|
            CMath::acos n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>cosh</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcCosh" => vectorize_monad { |inst, n|
            CMath::acosh n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>sin</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcSin" => vectorize_monad { |inst, n|
            CMath::asin n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>sinh</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcSinh" => vectorize_monad { |inst, n|
            CMath::asinh n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>tan</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcTan" => vectorize_monad { |inst, n|
            CMath::atan n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>tanh</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcTanh" => vectorize_monad { |inst, n|
            CMath::atanh n
        },
        #<<
        # Calculates the principal value of <math xmlns="http://www.w3.org/1998/Math/MathML"><msup><mi>tan</mi><mrow><mo>-</mo><mn>1</mn></mrow></msup><mo>(</mo><mi>y</mi><mo>/</mo><mi>x</mi><mo>)</mo></math>, or <code>atan2(y, x)</code>.
        # @type y number
        # @type x number
        # @return number
        # @genre numeric/trig
        #>>
        "ArcTan2" => vectorize_dyad { |inst, y, x|
            CMath::atan2 y, x
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>cos</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "Cos" => vectorize_monad { |inst, n|
            CMath::cos n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>cosh</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "Cosh" => vectorize_monad { |inst, n|
            CMath::cosh n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>sin</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "Sin" => vectorize_monad { |inst, n|
            CMath::sin n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>sinh</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "Sinh" => vectorize_monad { |inst, n|
            CMath::sinh n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>tan</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "Tan" => vectorize_monad { |inst, n|
            CMath::tan n
        },
        #<<
        # Calculates <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>tan</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>.
        # @type n number
        # @return number
        # @genre numeric/trig
        #>>
        "Tanh" => vectorize_monad { |inst, n|
            CMath::tanh n
        },

        ##-----------------##
        ## Prime Functions ##
        ##-----------------##
        #<<
        # Returns <code>true</code> if <code>n</code> is prime, and <code>false</code> otherwise.
        # @type n number
        # @return bool
        # @genre numeric/prime
        #>>
        "IsPrime" => vectorize_monad { |inst, n|
            Prime.prime? n
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is composite (not prime), and <code>false</code> otherwise.
        # @type n number
        # @return bool
        # @genre numeric/prime
        #>>
        "IsComposite" => vectorize_monad { |inst, n|
            !Prime.prime? n
        },
        #<<
        # Returns the next prime greater than <code>n</code>.
        # @return number
        # @type n number
        # @type rep number
        # @optional rep
        # @param rep When specified, returns the <code>rep</code>th prime after <code>n</code>.
        # @genre numeric/prime
        #>>
        "NextPrime" => vectorize_dyad { |inst, n, rep=1|
            rep.times {
                n += 1 + n % 2
                n += 2 until Prime.prime? n
            }
            n
        },
        #<<
        # Returns the closest previous prime before <code>n</code>.
        # @return number
        # @type n number
        # @type rep number
        # @optional rep
        # @param rep When specified, returns the <code>rep</code>th prime before <code>n</code>.
        # @genre numeric/prime
        #>>
        "PreviousPrime" => vectorize_dyad { |inst, n, rep=1|
            rep.times {
                break n = nil if n <= 2
                if n == 3
                    n = 2
                else
                    n -= 1 + n % 2
                    n -= 2 until Prime.prime? n
                end
            }
            n
        },
        #<<
        # Returns the <code>n</code>th prime, starting at <Code>Prime[1] = 2</code>.
        # @type n number
        # @return number
        # @genre numeric/prime
        # @example Print[Prime[1:10]]
        #>>
        "Prime" => vectorize_monad { |inst, n|
            nth_prime n
        },
        #<<
        # Returns a list of the prime exponents of <code>n</code>. That is, a list of base-exponent pairs <code>[[p1, e1], ..., [pN, eN]]</code> such that <math xmlns="http://www.w3.org/1998/Math/MathML"><msub><mi>p</mi><mi>k</mi></msub><mo>&#x2208;</mo><mi mathvariant="normal">&#x2119;</mi></math> and <math xmlns="http://www.w3.org/1998/Math/MathML"><mi>n</mi><mo>=</mo><munder><mo>&#x220F;</mo><mrow><mn>1</mn><mo>&#x2264;</mo><mi>k</mi><mo>&lt;</mo><mi>n</mi></mrow></munder><msubsup><mi>p</mi><mi>k</mi><msub><mi>e</mi><mi>k</mi></msub></msubsup></math>.
        # @return [[number, number]]
        # @type n number
        # @genre numeric/prime
        #>>
        "PrimeDivision" => vectorize_monad { |inst, n|
            Prime.prime_division n
        },
        #<<
        # Returns a list of the prime factors of <code>n</code> with duplicates.
        # @return [number]
        # @type n number
        # @genre numeric/prime
        #>>
        "PrimeFactors" => vectorize_monad { |inst, n|
            prime_factors n
        },
        #<<
        # Returns a list of the first <code>n</code> primes.
        # @return [number]
        # @type n number
        # @genre numeric/prime
        #>>
        "Primes" => vectorize_monad { |inst, n|
            Prime.first n
        },
        #<<
        # Returns the number of unique prime factors of <code>n</code>.
        # @return number
        # @type n number
        # @genre numeric/prime
        #>>
        "PrimeNu" => vectorize_monad { |inst, n|
            prime_factors(n).uniq.size
        },
        #<<
        # Returns the number of prime factors of <code>n</code>.
        # @return number
        # @type n number
        # @genre numeric/prime
        #>>
        "PrimeOmega" => vectorize_monad { |inst, n|
            prime_factors(n).size
        },

        ##------------------------##
        ## Number Logic Functions ##
        ##------------------------##
        #<<
        # Returns <code>true</code> if <code>n</code> is even (a multiple of <code>2</code>), otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Even" => vectorize_monad { |inst, n|
            n.even?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> = <code>a+bi</code> for <code>b /= 0</code>, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Imaginary" => vectorize_monad { |inst, n|
            @@functions["IsImaginary"][inst, n]
        },
        #<<
        # Returns <code>true</code> if <code>n</code> = <code>a+bi</code> for <code>b /= 0</code>, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "IsImaginary" => lambda { |inst, n|
            if Numeric === n
                unless n.real?
                    n.imaginary != 0
                else
                    false
                end
            else
                false
            end
        },
        #<<
        # Returns the imaginary part of <code>n</code>, or <code>0</code> if it doesn't exist.
        # @type n (*)
        # @return number
        # @genre numeric
        #>>
        "Im" => vectorize_monad { |inst, n|
            n.imaginary rescue 0
        },
        #<<
        # Returns the imaginary part of <code>n</code>, or <code>n</code> if it doesn't exist.
        # @type n (*)
        # @return number
        # @genre numeric
        #>>
        "Re" => vectorize_monad { |inst, n|
            n.real rescue n
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is less than <code>0</code>, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Negative" => vectorize_monad { |inst, n|
            n.negative?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is odd (not a multiple of <code>2</code>), otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Odd" => vectorize_monad { |inst, n|
            n.odd?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is greater than <code>0</code>, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Positive" => vectorize_monad { |inst, n|
            n.positive?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> has no imaginary part, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Real" => vectorize_monad { |inst, n|
            unless n.real?
                n.imaginary == 0
            else
                true
            end
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is <code>0</code>, otherwise <code>false</code>.
        # @return bool
        # @type n number
        # @genre numeric/logic
        #>>
        "Zero" => vectorize_monad { |inst, n|
            n.zero?
        },
        #<<
        # Returns <code>true</code> if <code>n</code> represents an integer, otherwise <code>false</code>.
        # @type n number
        # @return bool
        # @genre numeric/logic
        #>>
        "Integral" => vectorize_monad { |inst, n|
            n == n.floor
        },
        #<<
        # Returns <code>true</code> if <code>n</code> is a numeric value, otherwise <code>false</code>.
        # @type n number
        # @return bool
        # @genre numeric/logic
        #>>
        "Numeric" => lambda { |inst, n|
            Numeric === n
        },
        "IsSquare" => lambda { |inst, n|
            n.sqrt.to_i ** 2 == n
        },

        ##############################
        #### FUNCTIONAL FUNCTIONS ####
        ##############################
        #<<
        # Returns a function <code>fn[...args]</code> such that the <code>cond[...args]</code>th element of <code>flist</code> is called upon <code>args</code>.
        # @return fn[(*) -> (*)]
        # @type flist fn
        # @param flist A list of functions.
        # @type cond fn[(*) -> number]
        # @param cond A function which produces an index by which to obtain a function from <code>flist</code>.
        # @example f := Agenda[ [Halve, {3 * _ + 1}], Odd]
        # @example Print[Map[f, [1, 2, 3, 4]]]
        # @example ?? [4, 1, 10, 2]
        # @genre functional
        #>>
        "Agenda" => curry(3) { |inst, flist, cond, *args|
            unless AtState.func_like? cond
                ind = cond
            else
                ind = from_numlike cond[inst, *args]
            end

            flist[ind][inst, *args]
        },
        #<<
        # Calls <code>func</code> with <code>arg_arr</code>.
        # @type func fn
        # @type arg_arr [(*)]
        # @return (*)
        # @example args := [ Reverse, [ "hello", "world" ] ]
        # @example Print[ Apply[Map, args] ]
        # @example ?? ["olleh", "dlrow"]
        # @genre functional
        #>>
        "Apply" => lambda { |inst, func, arg_arr|
            func[inst, *arg_arr]
        },
        #<<
        # Returns a function <code>fn[...args]</code> which applies <code>func</code> to <code>args</code>.
        # @type func fn
        # @return fn[[(*)]]
        # @genre functional
        # @example f := Applier[Print]
        # @example f[1:5]
        # @example Print[1, 2, 3, 4, 5]
        # @example ?? 1 2 3 4 5
        #>>
        "Applier" => lambda { |inst, func|
            lambda { |inst, args|
                func[inst, *args]
            }
        },
        #<<
        # Bonds <code>larg</code> to the left side of <code>func</code>. That is, <code>Bond[func, larg][...args]</code> is the same as <code>func[larg, ...args]</code>.
        # @type func fn
        # @type larg (*)
        # @return fn
        # @genre functional
        #>>
        "Bond" => lambda { |inst, func, larg|
            lambda { |inst, *args|
                func[inst, larg, *args]
            }
        },
        #<<
        # Returns a function that returns <code>arg</code>.
        # @type arg (*)
        # @return fn
        # @genre functional
        #>>
        "C" => lambda { |inst, arg|
            lambda { |inst, *discard|
                arg
            }
        },
        #<<
        # Calls <code>f</code> over <code>args</code>.
        # @type args (*)
        # @type f fn
        # @return (*)
        # @genre functional
        # @example Call[Print, "Hello", "World"]
        # @example ?? Hello World
        #>>
        "Call" => lambda { |inst, f, *args|
            f[inst, *args]
        },
        #<<
        # Calls <code>f</code> <code>n</code> times over <code>args</code>, storing the results in a list.
        # @type args (*)
        # @type f fn
        # @type n number
        # @return (*)
        # @genre functional
        # @example Print[CallN[Random, 4, 3, 10]]
        # @example ?? 4 random integers between 3 and 10
        # @example ?? e.g.:
        # @example ?? [5, 8, 10, 9]
        #>>
        "CallN" => lambda { |inst, f, n, *args|
            res = []
            n.times {
                res << f[inst, *args]
            }
            res
        },
        #<<
        # Returns <code>fn</code>, fully curried according to <code>arity</code>.
        # @type f fn
        # @type arity number
        # @optional arity
        # @return fn
        # @param arity Default: <code>#f</code>.
        # @genre functional
        # @example add_dy := [x, y] -> { x + y }
        # @example adder := Curry[add_dy]
        # @example add3 := adder[3]
        # @example Print[add3[5]]
        # @example ?? 8
        # @example Print[adder[2][5]]
        # @example ?? 7
        # @example
        # @example add_tri := Curry[Add/3]
        # @example adder1 := add_tri[1]
        # @example add6 := adder1[5]
        # @example Print[add6[10]]
        # @example ?? 16
        #>>
        "Curry" => lambda { |inst, f, arity=f.arity|
            if arity.negative?
                arity = ~arity
            end

            arity += 1 if AtLambda === f || AtFunction === f

            rec = lambda { |fn, inst, *args, **opts|
                # parse arguments
                args.reject! { |e|
                    if ConfigureValue === e
                        opts[e.key.to_sym] = e.value
                        true
                    else
                        false
                    end
                }
                # p args, opts
                if args.size >= arity - 1
                    if AtState.configurable? fn
                        fn[inst, *args, **opts]
                    elsif opts.empty?
                        fn[inst, *args]
                    else
                        STDERR.puts "Excess parameter options in curried function: #{opts}"
                        fn[inst, *args]
                    end
                else
                    lambda { |inst, *more, **moreopts|
                        rec[fn, inst, *args, *more, **moreopts.merge(opts)]
                    }
                end
            }
            rec[f, inst]
        },
        #<<
        # Applies <code>f</code> to <code>n</code> until <code>f[n]</code> converges.
        # @type f fn
        # @type n (*)
        # @return (*)
        # @genre functional
        # @curries
        #>>
        "Fixpoint" => curry { |inst, f, n|
            fixpoint f.bind(inst), n
        },
        #<<
        # Composes the functions <code>f</code>, <code>g</code>, and <code>h</code> into a fork. When called with arguments <code>args</code>, this is equivalent to calling <code>g[f[...args], h[...args]]</code>.
        # @type f fn
        # @type g fn
        # @type h fn
        # @return fn
        # @genre functional
        # @example avg := Fork[Sum, Divide, Size]
        # @example Print[avg[1:5]]
        # @example ?? 3.0
        #>>
        "Fork" => lambda { |inst, f, g, h|
            lambda { |inst, *args|
                g[inst, f[inst, *args], h[inst, *args]]
            }
        },
        #<<
        # Composes the functions <code>f</code> and <code>g</code> into a hook. When called with arguments <code>args</code> this is equivalent to calling <code>f[First[args], g[...args]]</code>.
        # @type f fn
        # @type g fn
        # @return fn
        # @genre functional
        #>>
        "Hook" => lambda { |inst, f, g|
            lambda { |inst, *args|
                f[inst, args.first, g[inst, *args]]
            }
        },
        #<<
        # Given a function <code>func</code>, returns a function which calls <code>func</code> with an additional
        # argument representing the number of times <code>func</code> has been invoked.
        # @type func fn
        # @return fn
        # @genre functional
        #>>
        "InvocationIndex" => lambda { |inst, func|
            i = -1
            lambda { |inst, *args|
                func[inst, *args, @@functions["Succ"][inst, i]]
            }
        },
        #<<
        # Ties <code>args</code> together.
        # @type args (*)|fn
        # @return (*)|fn
        # @paramtype (*) args Concatenates all <code>args</code> together.
        # @paramtype fn args Creates a tie between all of <code>args</code>.
        # @genre functional
        # @example Print[Tie[1:3, 5:7]]
        # @example ?? [1, 2, 3, 5, 6, 7]
        # @example f := Tie[Double, Halve]
        # @example Print[f[1, 2, 3, 4, 5, 6]]
        # @example ?? [2, 1, 6, 2, 10, 3]
        #>>
        "Tie" => lambda { |inst, *args|
            if args.any? { |e| !AtState.func_like? e }
                args.inject([]) { |acc, e| [*acc, *e] }
            else
                Tie.new args
            end
        },
        #<<
        # <code>Tie</code>, but applied to the first argument instead of all arguments.
        # @type funcs fn
        # @genre functional
        # @return fn
        # @example f := TieArray[Double, Halve]
        # @example Print[f[1:6]]
        # @example ?? [2, 1, 6, 2, 10, 3]
        #>>
        "TieArray" => lambda { |inst, *funcs|
            Tie.new funcs, true
        },
        #<<
        # Applies <code>f</code> to <code>init</code> <code>n</code> times.
        # @type f fn
        # @type e (*)
        # @type n number
        # @return (*)
        # @genre functional
        # @example Print[Nest[Double, 1, 3]]
        # @example ?? 8 (= 2 ^ 3)
        #>>
        "Nest" => lambda { |inst, f, init, n|
            iter = init
            from_numlike(n).times {
                iter = f[inst, iter]
            }
            iter
        },
        #<<
        # Applies <code>f</code> to <code>init</code> <code>n</code> times, keeping the intermediate results.
        # @type f fn
        # @type e (*)
        # @type n number
        # @return (*)
        # @genre functional
        # @example Print[NestList[Double, 1, 3]]
        # @example ?? [1, 2, 4, 8]
        # @example Print[NestList[Double, 1, 3, first->false]]
        # @example ?? [2, 4, 8]
        # @option first Specifies whether or not to include the first element. Default: <code>true</code>.
        #>>
        "NestList" => configurable { |inst, f, init, n, **opts|
            first = get_default opts, :first, true
            iter = init
            list = first ? [iter] : []
            from_numlike(n).times {
                iter = f[inst, iter]
                list << iter
            }
            list
        },
        #<<
        # Applies <code>f</code> to <code>init</code> until <code>cond[init]</code> is truthy.
        # @type f fn
        # @type cond fn
        # @type init (*)
        # @return (*)
        # @genre functional
        # @example Print[NestWhile[Halve, 100, Even]]
        # @example ?? 25
        #>>
        "NestWhile" => lambda { |inst, f, init, cond|
            iter = init
            while cond[inst, iter]
                iter = f[inst, iter]
            end
            iter
        },
        #<<
        # Applies <code>f</code> to <code>init</code> until <code>cond[init]</code> is truthy.
        # @type f fn
        # @type cond fn
        # @type init (*)
        # @return (*)
        # @genre functional
        # @example Print[NestListWhile[Halve, 100, Even]]
        # @example ?? [100, 50, 25]
        # @option first whether or not the input element <code>init</code> is included as the first element in the results. Default: <code>true</code>.
        #>>
        "NestListWhile" => configurable { |inst, f, init, cond, **opts|
            first = get_default opts, :first, true
            iter = init
            list = first ? [iter] : []
            while cond[inst, iter]
                iter = f[inst, iter]
                list << iter
            end
            list
        },
        #<<
        # Applies <code>f</code> to indices of <code>arr</code>, selected by <code>cond</code>.
        # @type cond fn|[number]
        # @type f fn
        # @type arr [(*)]
        # @paramtype fn cond If the given index yields true over <code>cond</code>, then the respective element is mapped to <code>f</code>.
        # @paramtype [number] cond If the given index is a member of <code>cond</code>.
        # @genre functional
        # @return [number]
        # @example on_second := On[Odd]
        # @example rev_second := on_second[Reverse]
        # @example Print[rev_second[[
        # @example     [1, 2, 3],
        # @example     [4, 5, 6],
        # @example     ["a", "b", "c"],
        # @example     ["d", "e", "f"]
        # @example ]]]
        # @example ?? [[1, 2, 3], [6, 5, 4], ["a", "b", "c"], ["f", "e", "d"]]
        #>>
        "On" => curry { |inst, cond, f, arr|
            unless AtState.func_like? cond
                old = [*cond]
                cond = lambda { |inst, e| old.include? e }
            end

            map_vector(inst, arr, with_index: true) { |inst, e, i|
                cond[inst, i] ? f[inst, e] : e
            }
        },
        "Over" => lambda { |inst, *opts|
            
            
            opts.map! { |opt|
                key = if String === opt.key
                    inst.get_variable opt.key
                else
                    opt.key
                end
                unless AtState.func_like? key
                    old = [*key]
                    key = lambda { |inst, e| old.include? e }
                end
                opt.key = key
                opt
            }
            
            lambda { |inst, arr|
                map_vector(inst, arr, with_index: true) { |inst, e, i|
                    fn = opts.find { |opt| opt.key[inst, i] }
                    if fn
                        fn.value[inst, e]
                    else
                        e
                    end
                    
                }
            }
        },
        #<<
        # Returns a function which, given <code>(*) x</code>, applies <code>f</code> to <code>x</code> until a result occurs twice, then returns the list of intermediate steps.
        # @type f fn
        # @return fn
        # @genre functional
        # @example dig_root_steps := PeriodicSteps[Sum@Digits]
        # @example Print[dig_root_steps[1853]]
        # @example ?? [1853, 17, 8, 8]
        #>>
        "PeriodicSteps" => lambda { |inst, f|
            lambda { |inst, x|
                periodicloop f.bind(inst), x
            }
        },
        #<<
        # Returns a function which, given <code>(*) x</code>, applies <code>f</code> to <code>x</code> until a result occurs twice, then returns the final result.
        # @type f fn
        # @return fn
        # @genre functional
        # @example dig_root := Periodic[Sum@Digits]
        # @example Print[dig_root[1853]]
        # @example ?? 8
        #>>
        "Periodic" => lambda { |inst, f|
            # p "PERIODOC #{f}"
            lambda { |inst, x|
                periodicloop(f.bind(inst), x).last
            }
        },
        #<<
        # Bonds <code>larg</code> to the right side of <code>func</code>. That is, <code>Bond[func, rarg][...args]</code> is the same as <code>func[...args, rarg]</code>.
        # @type func fn
        # @type rarg (*)
        # @return fn
        # @genre functional
        #>>
        "RBond" => lambda { |inst, func, rarg|
            lambda { |inst, *args|
                func[inst, *args, rarg]
            }
        },


        #########################
        #### LOGIC FUNCTIONS ####
        #########################
        #<<
        # Returns <code>true</code> if all members of <code>Map[f, list]</code> are truthy.
        # @optional list
        # @type list [(*)]
        # @type f fn|[(*)]
        # @param list When omitted, returns <code>true</code> if all members of <code>f</code> are truthy. Otherwise, returns <code>false</code>. When specified, <code>f</code> <em>must</em> be a function.
        # @return bool
        # @genre logic
        #>>
        "All" => lambda { |inst, f, list=nil|
            if list.nil?
                f.all? { |e| AtState.truthy? e }
            else
                list.all? { |e| AtState.truthy?(f[inst, e]) }
            end
        },
        #<<
        # Returns <code>true</code> if any member of <code>Map[f, list]</code> is truthy.
        # @optional list
        # @type list [(*)]
        # @type f fn|[(*)]
        # @param list When omitted, returns <code>true</code> if any member of <code>f</code> is truthy. Otherwise, returns <code>false</code>. When specified, <code>f</code> <em>must</em> be a function.
        # @return bool
        # @genre logic
        #>>
        "Any" => lambda { |inst, f, list=nil|
            if list.nil?
                f.any? { |e| AtState.truthy? e }
            else
                list.any? { |e| AtState.truthy?(f[inst, e]) }
            end
        },
        #<<
        # Returns <code>Not[Any[...]]</code>.
        # @optional list
        # @type list [(*)]
        # @type f fn|[(*)]
        # @param list When omitted, returns <code>true</code> if any member of <code>f</code> is truthy. Otherwise, returns <code>false</code>. When specified, <code>f</code> <em>must</em> be a function.
        # @return bool
        # @genre logic
        #>>
        "None" => lambda { |inst, f, list=nil|
            !@@functions["Any"][inst, f, list]
        },
        #<<
        # Returns <code>true</code> if <code>arg</code> is falsey, <code>false</code> otherwise. (See also: <a href="#Falsey"><code>Falsey</code></a>.)
        # @return bool
        # @type arg (*)
        # @genre logic
        #>>
        "Not" => lambda { |inst, arg|
            AtState.falsey? arg
        },
        #<<
        # Returns <code>true</code> if <code>arg</code> is falsey, <code>false</code> otherwise. (See also: <a href="#Not"><code>Not</code></a>.)
        # @return bool
        # @type arg (*)
        # @genre logic
        #>>
        "Falsey" => lambda { |inst, arg|
            AtState.falsey? arg
        },
        "Cases" => held(HOLD_ALL) { |inst, *pairs|
            res = pairs.map { |config|
                config.children
            }.find { |cond, value|
                AtState.truthy? inst.evaluate_node cond
            }
            if res.nil?
                res
            else
                inst.evaluate_node res[1]
            end
        },
        #<<
        # If <code>cond</code> is truthy, evaluates <code>t</code>. Otherwise, if <code>f</code> is specified, evaluates <code>f</code>.
        # @return (*)
        # @type cond (*)
        # @type t expression
        # @type f expression
        # @optional f
        # @genre logic
        # @example If[3 = 4,
        # @example     Print["Logic does not hold!"],
        # @example     Print["Everything is situation normal."]
        # @example ]
        #>>
        "If" => lambda { |inst, cond, t, f=nil|
            res = if AtState.truthy? cond
                t
            else
                f
            end
            inst.evaluate_node_safe res
        },
        #<<
        # Determines whether or not <code>arg = func[arg]</code>
        # @return bool
        # @genre logic
        # @type arg (*)
        # @type func fn
        # @optional arg
        # @param arg when omitted, returns a function <code>f[x] = Invariant[func, x]</code>.
        #>>
        "Invariant" => lambda { |inst, func, arg=nil|
            if arg.nil?
                lambda { |inst, arg|
                    func[inst, arg] == arg
                }
            else
                func[inst, arg] == arg
            end
        },
        #<<
        # Selects all elements <code>el</code> in <code>list</code> whose respective member in <code>mask</code> is truthy.
        # @type mask [(*)]
        # @type list [(*)]
        # @return [(*)]
        # @example Print[Mask[ [true, false, true, true, false], 1:5]]
        # @example ?? [1, 3, 4]
        # @genre logic
        # @reforms
        #>>
        "Mask" => lambda { |inst, mask, list|
            masked = inst.cast_list(list).select.with_index { |e, i|
                AtState.truthy? mask[i]
            }
            reform_list masked, list
        },
        "Reform" => lambda { |inst, result, to|
            reform_list result, to
        },
        #<<
        # Returns <code>true</code> if <code>arg</code> is truthy, <code>false</code> otherwise.
        # @return bool
        # @type arg (*)
        # @genre logic
        #>>
        "Truthy" => lambda { |inst, arg|
            AtState.truthy? arg
        },
        #<<
        # While <code>cond</code> evaluates as truthy, evaluates <code>body</code>. Returns <code>nil</code> if <code>cond</code> was false before executing <code>body</code>.
        # @return (*)
        # @type cond expression
        # @type body expression
        # @genre logic
        # @example i := 0
        # @example While[i < 5, Print[i]; i := i + 1]
        # @example ?? 0
        # @example ?? 1
        # @example ?? 2
        # @example ?? 3
        # @example ?? 4
        # @example While[false, Print["Hello!"]]
        # @example ?? nothing is printed
        #>>
        "While" => lambda { |inst, cond, body|
            res = nil
            loop {
                # dhash "while scope", inst.locals.last
                c = inst.evaluate_node cond
                unless AtState.truthy? c
                    break
                end
                res = inst.evaluate_node body
                # p body
                # dhash "after scope", inst.locals.last
                # STDIN.gets
            }
            res
        },
        #<<
        # Evaluates <code>body</code>, then stops only if <code>cond</code> evaluates as falsey.
        # @return (*)
        # @type cond expression
        # @type body expression
        # @genre logic
        # @example i := 0
        # @example DoWhile[i < 5, Print[i]; i := i + 1]
        # @example ?? 0
        # @example ?? 1
        # @example ?? 2
        # @example ?? 3
        # @example ?? 4
        # @example DoWhile[false, Print["Hello!"]]
        # @example ?? Hello!
        #>>
        "DoWhile" => lambda { |inst, cond, body|
            res = nil
            loop {
                res = inst.evaluate_node body
                c = inst.evaluate_node cond
                unless AtState.truthy? c
                    break
                end
            }
            res
        },
        #<<
        # For every value <code>el</code> in <code>ent</code>, evaluates <code>body</code>, setting the first abstract value to <code>el</code>, and the second to its index.
        # @genre logic
        # @type ent [(*)]
        # @return nil
        #>>
        "ForEach" => lambda { |inst, ent, body|
            arr = inst.cast_list(ent)

            arr.each_with_index { |x, i|
                inst.evaluate_node body, [x, i]
            }

            nil
        },


        ########################
        #### LIST FUNCTIONS ####
        ########################
        #<<
        # Generates the cumulative sums of <code>list</code>.
        # @type list [number]
        # @return [number]
        # @genre list
        #>>
        "Accumulate" => lambda { |inst, list|
            list.prefixes.map { |e| @@functions["Sum"][inst, e] }
        },
        #<<
        # Flattens the matrices held in the matrix-like <code>list</code>.
        # @type list [[list]]
        # @return [list]
        # @genre list/matrix
        # @example m1 := [[1, 2], [3, 4]]
        # @example m2 := [[0, 0], [7, 7]]
        # @example Display[ArrayFlatten[ [[m1, m2, m1], [m2, m1, m2]] ]]
        # @example ??  1 2 0 0 1 2
        # @example ??  3 4 7 7 3 4
        # @example ??  0 0 1 2 0 0
        # @example ??  7 7 3 4 7 7
        #>>
        "ArrayFlatten" => lambda { |inst, list|
            inner = list.flatten(1).first { |e| Array === e }

            size = [*dim(inner)]
            if size.size < 2
                list
            else
                list.map { |row|
                    row.inject { |acc, mat|
                        unless Array === mat
                            mat = mat_from(*size, mat)
                        end
                        stitch acc, mat
                    }
                }.flatten(1)
            end
        },
        #<<
        # Returns the average of <code>list</code>, that is, the sum of the elements divided by the length.
        # @type list [number]
        # @return number
        # @genre list
        #>>
        "Average" => lambda { |inst, list|
            list.average
        },
        #<<
        # Splits <code>list</code> in half.
        # @type list [(*)]
        # @return [[(*)], [(*)]]
        # @genre list
        # @option bias what should be done with the center element, in the case of odd lists. <code>"none"</code>: drop it (default). <code>"left"</code>: append center to the left half. <code>"right"</code>: append center to the right half.
        # @example Print[Bisect[1:4]]
        # @example ?? [[1, 2], [3, 4]]
        # @example Print[Bisect[1:5]]
        # @example ?? [[1, 2], [4, 5]]
        # @example Print[Bisect[1:5, bias->$none]]
        # @example ?? [[1, 2], [4, 5]]
        # @example Print[Bisect[1:5, bias->$left]]
        # @example ?? [[1, 2, 3], [4, 5]]
        # @example Print[Bisect[1:5, bias->$right]]
        # @example ?? [[1, 2], [3, 4, 5]]
        #>>
        "Bisect" => configurable(arity: 1) { |inst, list, **opts|
            opts[:bias] ||= "none"
            right_upper = list.size / 2.0
            left_lower = list.size / 2.0

            case opts[:bias]
                when "none"
                    right_upper = right_upper.floor
                    left_lower = left_lower.ceil

                when "left"
                    right_upper = right_upper.ceil
                    left_lower = left_lower.ceil

                when "right"
                    right_upper = right_upper.floor
                    left_lower = left_lower.floor

                else
                    STDERR.puts "Invalid option to `Bisect`: #{opts[:bias].inspect}"
            end

            [list[0...right_upper], list[left_lower..-1]]
        },
        #<<
        # "Bounces" a list. That is, returns the list concatenated with itself reversed, where the last element of the original array is not duplicated in the concatenation. Inspiration: <a href="https://github.com/DennisMitchell/jellylanguage/wiki/Atoms#%C5%92-atoms-other-monads">Jelly's <code>ŒB</code> monadic atom</code></a>.
        # @type list [(*)]
        # @return [(*)]
        # @reforms
        # @genre list
        # @example Print[Bounce[1:3]]
        # @example ?? [1, 2, 3, 2, 1]
        # @example Print[Bounce["Hello"]]
        # @example ?? HellolleH
        # @example Print[Bounce[942]]
        # @example ?? 94249
        #>>
        "Bounce" => lambda { |inst, list|
            listified = inst.cast_list(list)
            bounced = listified[0..-2] + listified.reverse
            reform_list bounced, list
        },
        #<<
        # Chops <code>list</code> into groups of length <code>size</code>.
        # @type list [(*)]
        # @return [[(*)]]
        # @option extra Boolean: keeps elements which don't add up to <code>size</code> if <code>true</code>. Default: <code>true</code>.
        # @genre list
        # @example Print[Chop[1:8, 3]]
        # @example ?? [[1, 2, 3], [4, 5, 6], [7, 8]]
        # @example Print[Chop[1:8, 3], extra->false]
        # @example ?? [[1, 2, 3], [4, 5, 6]]
        # @example Print[Chop["Hello, World!", 3]]
        # @example ?? ["Hel", "lo,", " Wo", "rld", "!"]
        # @example Print[Chop[1:12, [1, 2, 3]]]
        # @example ?? [[1], [2, 3], [4, 5, 6], [7], [8, 9], [10, 11, 12]]
        # @reforms elements
        #>>
        "Chop" => lambda { |inst, list, size, **opts|
            extra = get_default opts, :extra, true
            res = chop inst.cast_list(list), size, extra
            res.map { |e|
                reform_list e, list
            }
        },
        #<<
        # Concatenates all the lists in <code>lists</code>, except for matching prefix-suffix pairs.
        # @type lists [(*)]
        # @return [(*)]
        # @genre list
        #>>
        "Collapse" => lambda { |inst, *lists|
            lists.map.with_index { |c, i|
                if i == 0
                    c
                else
                    prev = lists[i - 1]
                    minsize = [prev, c].map(&:size).min

                    ind = (0..minsize).reverse_each.find { |size|
                        offset = prev.size - size
                        (0...size).all? { |j|
                            c[j] == prev[offset + j]
                        }
                    }
                    c[ind..-1]
                end
            }.flatten(1)
        },
        #<<
        # Chunks <code>list</code> into runs of consecutive values. Returns an array of members which look like <code>[el, els]</code>, where <code>el</code> is the principal run value, and <code>els</code> are the values in that run.
        # @type list [(*)]
        # @optional f
        # @type f fn
        # @param f When specified, maps each value <code>el</code> in <code>list</code> over <code>f</code>, then uses <code>f[el]</code> as the principal run value.
        # @return [[(*), [(*)]]]
        # @genre list
        # @example Print[Chunk[ [1, 2, 3, 2, 2, 3] ]]
        # @example ?? [[1, [1]], [2, [2]], [3, [3]], [2, [2, 2]], [3, [3]]]
        # @example Display[Chunk[ [1, -1, 1, 1, 2, 3, -3, 5, -2 ], Square ]]
        # @example ??   1 [1, -1, 1, 1]
        # @example ??   4           [2]
        # @example ??   9       [3, -3]
        # @example ??  25           [5]
        # @example ??   4          [-2]
        #>>
        "Chunk" => lambda { |inst, list, f=nil|
            list = inst.cast_list list
            if f.nil?
                list.chunk { |e| e }.to_a
            else
                list.chunk { |e| f[inst, e] }.to_a
            end
        },
        #<<
        # Returns all elements of <code>parent</code> not included in any of the lists in <code>args</code>.
        # @type parent [(*)]
        # @type args [(*)]
        # @return [(*)]
        # @genre list
        #>>
        "Complement" => lambda { |inst, parent, *args|
            any = args.map { |e|
                inst.cast_list e
            }.flatten(1)
            inst.cast_list(parent).reject { |e| any.include? e }
        },
        #<<
        # Returns the concatentation of each list in <code>args</code>.
        # @type args [(*)]
        # @return [(*)]
        # @genre list
        # @example Print[Concat[ 1:5, 2:3, [7], [[8, 9]] ]]
        # @example ?? [1, 2, 3, 4, 5, 2, 3, 7, [8, 9]]
        #>>
        "Concat" => lambda { |inst, *args|
            args.flatten(1)
        },
        #<<
        # Counts the number of members satisfying <code>f</code> in <code>list</code>; or, if <code>f</code> is not a function, the number of times <code>f</code> appears in <code>list</code>.
        # @type list [(*)]
        # @type f fn|(*)
        # @return number
        # @genre list
        #>>
        "Count" => curry { |inst, f, list|
            list = inst.cast_list list
            if f.is_a?(Proc) || f.is_a?(AtLambda)
                list.count { |e| f[inst, e] }
            else
                list.count f
            end
        },
        #<<
        # Returns <code>true</code> if every element <code>a</code> which is followed by an element <code>b</code> is strictly greater than <code>b</code>.
        # @type list [(*)]
        # @return bool
        # @genre list/logic
        # @example Print[Decreasing[ -(1:5) ]]
        # @example ?? true
        # @example Print[Decreasing[ [2, 1, 0] ]]
        # @example ?? true
        # @example Print[Decreasing[ [] ]]
        # @example ?? true
        # @example Print[Decreasing[ [2, 2, 1, 0] ]]
        # @example ?? false
        # @example Print[Decreasing[ 1:5 ]]
        # @example ?? false
        #>>
        "Decreasing" => lambda { |inst, list|
            inst.cast_list(list).delta.all?(&:negative?) rescue false
        },
        #<<
        # Returns the differences between each member of <code>list</code>.
        # @type list [number]
        # @return [number]
        # @genre list
        # @example Print[Delta[ 1:4 ]]
        # @example ?? [1, 1, 1]
        # @example Print[Delta[ [] ]]
        # @example ?? []
        # @example Print[Delta[ Square[0:4] ]]
        # @example ?? [1, 3, 5, 7]
        #>>
        "Delta" => lambda { |inst, list|
            list.delta
        },
        #<<
        # Returns all elements exclusive to <code>a</code> or <code>b</code>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre list
        #>>
        "Difference" => lambda { |inst, a, b|
            halve1 = @@functions["Complement"][inst, a, b]
            halve2 = @@functions["Complement"][inst, b, a]
            halve1 + halve2
        },
        #<<
        # Returns a list of all elements in <code>list</code> paired with their indices,
        # starting at <code>offset</code>.
        # @type list [(*)]
        # @return [[(*), number]]
        # @genre list
        #>>
        "Enumerate" => lambda { |inst, list, start=0|
            i = start
            list.map { |e|
                res = [e, i]
                i = @@functions["Succ"][inst, i]
                res
            }
        },
        #<<
        # Returns the first element of <code>list</code>.
        # @type list [(*)]
        # @return (*)
        # @genre list
        # @example Print[First[ [3, 9, 2, 0] ]]
        # @example ?? 3
        # @example Print[First[ "Hello!" ]]
        # @example ?? H
        #>>
        "First" => lambda { |inst, list|
            inst.cast_list(list)[0]
        },
        #<<
        # Returns the first element <code>e</code> in <code>list</code> which satisfies <code>f</code>. Returns <code>nil</code> otherwise.
        # @type f fn|(*)
        # @type list [(*)]
        # @return (*)
        # @paramtype fn f applied to each successive member of <code>list</code> until a truthy value is obtained.
        # @paramtype (*) f returns the first element in <code>list</code> equal to <code>f</code>.
        # @genre list
        #>>
        "Find" => lambda { |inst, f, list|
            if AtState.func_like? f
                list.find { |e| f[inst, e] }
            else
                list.find { |e| e == f }
            end
        },
        #<<
        # Flattens <code>list</code>.
        # @type list [(*)]
        # @optional n
        # @type n number
        # @return [(*)]
        # @genre list
        # @param n when specified, flattens <code>list</code> to 1 level <code>n</code> times.
        #>>
        "Flat" => lambda { |inst, list, n=nil|
            list.flatten(n)
        },
        #<<
        # Reverses and transposes a list, or, if the list has only one dimension, reverses it.
        # @type list [(*)]
        # @return [(*)]
        # @reforms
        # @genre list
        #>>
        "Flip" => lambda { |inst, list|
            is_string = String === list
            inner = is_string ? @@functions["Grid"][inst, list] : inst.cast_list(list)


            result = begin
                inner.transpose.reverse
            rescue
                inner.reverse
            end

            is_string ? @@functions["UnGrid"][inst, result] : reform_list(result, list)
        },
        #<<
        # Gets all members at indices <code>inds</code> from list.
        # @type list [(*)]
        # @type ind number|ConfigureValue
        # @return [(*)]|string
        # @paramtype ConfigureValue ind Returns the members between the key and the value. If the value is negative, starts at the respective index from the right. Returns a string if given a string, instead of an array of characters.
        # @genre list
        # @example Print[Get[1:5, 2]]
        # @example ?? 3
        # @example Print[Get["Hello, World!", 0:4]]
        # @example ?? ["H", "e", "l", "l", "o"]
        # @example Print[Get["Hello, World!", 4 -> -4]]
        #>>
        "Get" => vectorize_dyad(RIGHT) { |inst, list, ind|
            if Hash === list
                list[ind]
            else
                ind = force_number ind
                if ConfigureValue === ind
                    list[ind.key..ind.value]
                elsif class_has? list, "$get"
                    list["$get"][inst, list, ind]
                else
                    list[ind]
                end
            end
        },
        #<<
        # Returns <code>true</code> if <code>list</code> contains <code>member</code>, <code>false</code> otherwise.
        # @type list [(*)]|string
        # @type member (*)
        # @return bool
        # @genre list
        # @paramtype string list returns if <code>member</code> is contained within <code>list</code>.
        #>>
        "Has" => lambda { |inst, list, member|
            if String === list
                !!list.index(member)
            else
                list.include? member
            end
        },
        #<<
        # Returns <code>list[i0][i1]...[iN]</code> for each <code>i</code> in <code>inds</code>.
        # @type list [(*)]
        # @type inds [(*)]|(*)
        # @return (*)
        # @genre list
        # @example id := Identity[3]
        # @example Display[id]
        # @example ??  1 0 0
        # @example ??  0 1 0
        # @example ??  0 0 1
        # @example Print[FlatGet[id, 0]]
        # @example ?? [1, 0, 0]
        # @example Print[FlatGet[id, [0]]]
        # @example ?? [1, 0, 0]
        # @example Print[FlatGet[id, [0, 0]]]
        # @example ?? 1
        #>>
        "FlatGet" => lambda { |inst, list, inds|
            inst.enlist(inds).each { |i|
                list = list[i]
            }
            list
        },
        #<<
        # Returns a list of indices such that, when <code>list</code> is indexed by that list, the result is <code>list</code> sorted.
        # @type list [(*)]
        # @return [number]
        # @genre list
        #>>
        "Grade" => lambda { |inst, list|
            grade inst.cast_list list
        },
        #<<
        # Groups <code>list</code> according to <code>Map[fn, list]</code>.
        # @type list [(*)]
        # @type f fn
        # @return Hash[(*) -> (*)]
        # @genre list
        #>>
        "Group" => curry { |inst, f, list|
            inst.cast_list(list).group_by { |e| f[inst, e] }
        },
        #<<
        # Equivalent to <code><a href="#UnSparse">UnSparse</a>@<a href="#Group">Group</a></code>.
        # @type list [(*)]
        # @type f fn
        # @type fill (*)
        # @return [(*)]
        # @optional fill
        # @genre list
        #>>
        "FlatGroup" => curry { |inst, f, list, fill=nil|
            @@functions["UnSparse"][inst,
                @@functions["Group"][inst, f, list],
                fill
            ]
        },
        #<<
        # Returns <code>true</code> if each member <code>el</code> is strictly greater than the previous element, otherwise <code>false</code>.
        # @type list [(*)]
        # @return bool
        # @genre list/logic
        #>>
        "Increasing" => lambda { |inst, list|
            inst.cast_list(list).delta.all?(&:positive?) rescue false
        },
        #<<
        # Returns all indices at which <code>ind</code> occurs in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return [number]
        # @genre list
        # @example Print[Indices[ [1, 1, 2, 3, 1, 3, 1], [1, 3] ]]
        # @example ?? [[0, 1, 4, 6], [3, 5]]
        #>>
        "Indices" => vectorize_dyad(RIGHT) { |inst, list, ind|
            inst.cast_list(list).indices ind
        },
        #<<
        # Returns the index of the first occurrence of <code>ind</code> in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return number
        # @genre list
        #>>
        "Index" => vectorize_dyad(RIGHT) { |inst, list, ind|
            if String === list
                list.index ind
            else
                inst.cast_list(list).index ind
            end
        },
        #<<
        # Returns the index of the last occurrence of <code>ind</code> in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return number
        # @genre list
        #>>
        "LastIndex" => vectorize_dyad(RIGHT) { |inst, list, ind|
            if String === list
                list.rindex ind
            else
                inst.cast_list(list).rindex ind
            end
        },
        #<<
        # Returns all indices at which <code>ind</code> occurs in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return [number]
        # @genre list
        # @example Print[IndicesFlat[ [1, 1, 2, 3, 1, 3, 1], [1, 3] ]]
        # @example ?? []
        #>>
        "IndicesFlat" => lambda { |inst, list, ind|
            inst.cast_list(list).indices ind
        },
        #<<
        # Returns the index of the first occurrence of <code>ind</code> in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return number
        # @genre list
        #>>
        "IndexFlat" => lambda { |inst, list, ind|
            inst.cast_list(list).index ind
        },
        #<<
        # Returns the index of the last occurrence of <code>ind</code> in <code>list</code>.
        # @type list [(*)]
        # @type ind (*)
        # @return number
        # @genre list
        #>>
        "LastIndexFlat" => lambda { |inst, list, ind|
            inst.cast_list(list).rindex ind
        },
        #<<
        # Returns the first <code>Prod[Size => args]</code> non-negative integers.
        # @type number args
        # @return [(*)]
        # @genre list
        #>>
        "Integers" => lambda { |inst, *args|
            args = args.flat_map { |e| e }
            size = args.prod
            iter = args[1..-1]
            res = (0...size).to_a

            until iter.empty?
                res = @@functions["Chop"][inst, res, iter.pop]
            end

            res
        },
        #<<
        # Returns the intersection of the arguments.
        # @type lists [(*)]
        # @return [(*)]
        # @genre list
        #>>
        "Intersection" => lambda { |inst, *lists|
            lists.inject(&:&)
        },
        #<<
        # Returns a list where <code>joiner</code> is placed in between each element of <code>list</code>.
        # @type list [(*)]
        # @type joiner (*)
        # @return [(*)]
        # @genre list
        # @example Print[Intersperse[1:3, 0]]
        # @example ?? [1, 0, 2, 0, 3]
        #>>
        "Intersperse" => lambda { |inst, list, joiner|
            res = []
            inst.cast_list(list).each_with_index { |e, i|
                res << e
                res << joiner if i != list.size - 1
            }
            res
        },
        #<<
        # Returns an range from <code>0</code> (inclusive) to <code>min</code> or the length of <code>min</code> as applicable (exclusive).
        # @type min [(*)]|number
        # @return [(*)]
        # @genre list
        # @example Print[Iota[5]]
        # @example ?? [0, 1, 2, 3, 4]
        # @example Print[Iota["Hi!"]]
        # @example ?? [0, 1, 2]
        #>>
        "Iota" => lambda { |inst, min|
            ((0...min) rescue (0...min.size)).to_a
        },
        #<<
        # Returns the larger of <code>a</code> and <code>b</code>. Similar to <a href="#Max"><code>Max</code></a>, except <code>Larger</code> vectorizes.
        # @type a [(*)]
        # @type b [(*)]
        # @return (*)
        # @genre list
        # @example Print[Larger[1, 5]]
        # @example ?? 5
        # @example Print[Larger[[1, 2, 3], [3, 2, 1]]]
        # @example ?? [3, 2, 3]
        #>>
        "Larger" => vectorize_dyad { |inst, a, b|
            [*a, *b].max
        },
        #<<
        # Returns the last member of <code>list</code>.
        # @type list [(*)]|string
        # @return (*)
        # @genre list
        # @example Print[Last["hiya"]]
        # @example ?? a
        # @example Print[Last[1:5]]
        # @example ?? 5
        #>>
        "Last" => lambda { |inst, list|
            inst.cast_list(list)[-1]
        },
        #<<
        # Forces <code>ent</code> into a list.
        # @type ent (*)
        # @return [(*)]
        # @genre list
        # @paramtype number ent Returns the digits of <code>ent</code>.
        # @paramtype string ent Returns the characters of <code>ent</code>.
        # @paramtype hash ent Returns an array of key-value pairs in <code>ent</code>.
        #>>
        "List" => lambda { |inst, ent|
            inst.cast_list ent
        },
        #<<
        # Returns the largest element contained in any atom of <code>args</code>.
        # @type args (*)
        # @return (*)
        # @genre list
        #>>
        "Max" => lambda { |inst, *args|
            (args.flatten - [nil]).max
        },
        #<<
        # Returns the median of <code>list</code>.
        # @type list [number]
        # @return number
        # @genre list
        # @example Print[Median[[1, 2, 3]]]
        # @example ?? 2
        # @example Print[Median[[1, 2, 3, 4]]]
        # @example ?? 2.5
        #>>
        "Median" => lambda { |inst, list|
            force_number list.median
        },
        #<<
        # Returns the smallest element contained in any atom of <code>args</code>.
        # @type args (*)
        # @return (*)
        # @genre list
        #>>
        "Min" => lambda { |inst, *args|
            (args.flatten - [nil]).min
        },
        #<<
        # Returns the outermost <code>n</code> elements from the left and right sides of <code>list</code>.
        # @type list [(*)]
        # @type n number
        # @optional n
        # @return [(*)]
        # @param n Default: <code>1</code>
        # @genre list
        # @example Print[Outers[1:5]]
        # @example ?? [1, 5]
        # @example Print[Outers[1:5, 2]]
        # @example ?? [1, 2, 4, 5]
        #>>
        "Outers" => vectorize_dyad(RIGHT) { |inst, list, n=1|
            list[0...n] + list[-n..-1]
        },
        #<<
        # Returns <code>true</code> if <code>search</code> exists as a contiguous subset in <code>list</code>, otherwise <code>false</code>.
        # @type list [(*)]
        # @type search [(*)]
        # @return bool
        # @genre list/logic
        #>>
        "Overlap" => lambda { |inst, list, search|
            overlap list, search
        },
        #<<
        # Returns the non-empty prefixes of <code>list</code>.
        # @reforms elements
        # @type list [(*)]
        # @return [[(*)]]
        # @genre list
        # @example Print[Prefixes[1:5]]
        # @example ?? [[1], [1, 2], [1, 2, 3], [1, 2, 3, 4], [1, 2, 3, 4, 5]]
        # @example Print[Prefixes["hi?"]]
        # @example ?? ["h", "hi", "hi?"]
        # @example Print[Prefixes[901]]
        # @example ?? [9, 90, 901]
        #>>
        "Prefixes" => lambda { |inst, list|
            inst.cast_list(list).prefixes.map { |e|
                reform_list e, list
            }
        },
        #<<
        # Returns the non-empty suffixes of <code>list</code>.
        # @reforms elements
        # @type list [(*)]
        # @return [[(*)]]
        # @genre list
        # @example Print[Suffixes[1:5]]
        # @example ?? [[1, 2, 3, 4, 5], [2, 3, 4, 5], [3, 4, 5], [4, 5], [5]]
        # @example Print[Suffixes["hi?"]]
        # @example ?? ["hi?", "i?", "?"]
        # @example Print[Suffixes[901]]
        # @example ?? [901, 1, 1]
        #>>
        "Suffixes" => lambda { |inst, list|
            inst.cast_list(list).suffixes.map { |e|
                reform_list e, list
            }
        },
        #<<
        # Returns a list of lists of the form <code>[id, inds]</code>, where <code>id</code> corresponds to a unique element of <code>els</code>.
        # <code>inds</code> is a narray of indices where <code>id</code> occurs.
        # @type arr [(*)]
        # @type els [(*)]
        # @return [(*), [(*)]]
        # @optional els
        # @param els Default: <code>arr</code>.
        # @genre list
        # @example Display[Positions[ [1, 2, 3, 2, 4, 1] ]]
        # @example  1 [0, 5]
        # @example  2 [1, 3]
        # @example  3    [2]
        # @example  4    [4]
        # @example Display[Positions["Hello!", ["l", "!"]]]
        # @example "l" [2, 3]
        # @example "!"    [5]
        #>>
        "Positions" => lambda { |inst, arr, els=arr|
            arr = inst.cast_list arr
            els = inst.cast_list els
            positions arr, els
        },
        #<<
        # Returns the powerset of <code>list</code>.
        # @type list [(*)]
        # @return [[(*)]]
        # @genre list
        # @reforms elements
        # @example Print[Powerset[ [9,4,6] ]]
        # @example ?? [[], [9], [4], [9, 4], [6], [9, 6], [4, 6], [9, 4, 6]]
        # @example Print[Powerset[123]]
        # @example ?? [0, 1, 2, 12, 3, 13, 23, 123]
        #>>
        "Powerset" => lambda { |inst, list|
            inst.cast_list(list).powerset.map { |e|
                reform_list e, list
            }
        },
        #<<
        # Returns the product of all members of <code>list</code>.
        # @type list [number]
        # @return number
        # @genre list
        #>>
        "Prod" => lambda { |inst, list|
            list.inject(1) { |a, c|
                @@operators["*"][inst, a, c]
            }
        },
        #<<
        # Returns a range from <code>min</code> to <code>max</code> inclusive.
        # @type min number|string
        # @type max number|string
        # @return [number|string]
        # @genre list
        # @optional max
        # @param max If omitted, returns <code>Range[0, min]</code>.
        # @example Print[Range[1, 5]]
        # @example ?? [1, 2, 3, 4, 5]
        # @example Print[Range["a", "d"]]
        # @example ?? ["a", "b", "c", "d"]
        #>>
        "Range" => vectorize_dyad { |inst, min, max=nil|
            if max.nil?
                (0..min).to_a
            else
                (min..max).to_a
            end
        },
        #<<
        # Resizes <code>list</code> to be of length <code>size</code>, cyclically repeating elements if <code>size</code> exceeds <code>#list</code>.
        # @reforms
        # @type list [(*)]
        # @type size number
        # @genre list
        # @return [(*)]
        # @example Print[Resize["Hello!", 2]]
        # @example ?? He
        # @example Print[Resize[[1, 2], 7]]
        # @example ?? [1, 2, 1, 2, 1, 2, 1]
        #>>
        "Resize" => lambda { |inst, list, size|
            res = resize inst.cast_list(list), size
            reform_list res, list
        },
        #<<
        # Returns <code>list</code> without any instances of <code>ent</code>.
        # @reforms
        # @type list [(*)]
        # @type ent (*)
        # @genre list
        # @return [(*)]
        # @example Print[Remove[1:5, 3]]
        # @example ?? [1, 2, 4, 5]
        # @example Print[Remove["Hello, World!", "l"]]
        # @example ?? "Heo, Word!"
        # @example Print[Remove["Hello, World!", "ll"]]
        # @example ?? "Hello, World!"
        #>>
        "Remove" => lambda { |inst, list, ent|
            iter = inst.cast_list(list)
            iter.delete ent
            reform_list iter, list
        },
        #<<
        # Returns <code>list</code> without the first occurrence of <code>ent</code>.
        # @reforms
        # @type list [(*)]
        # @type ent (*)
        # @return [(*)]
        # @genre list
        # @example Print[RemoveFirst[[1, 2, 3, 3, 4, 4, 5], 3]]
        # @example ?? [1, 2, 3, 4, 4, 5]
        # @example Print[RemoveFirst["Hello, World!", "l"]]
        # @example ?? "Helo, World!"
        # @example Print[RemoveFirst["Hello, World!", "ll"]]
        # @example ?? "Hello, World!"
        #>>
        "RemoveFirst" => lambda { |inst, list, ent|
            iter = inst.cast_list(list)
            index = iter.index ent
            iter.delete_at index unless index.nil?
            reform_list iter, list
        },
        #<<
        # Returns <code>list</code> without the first occurrence of <code>ent</code>.
        # @reforms
        # @type list [(*)]
        # @type ent (*)
        # @return [(*)]
        # @genre list
        # @example Print[RemoveLast[[1, 2, 3, 3, 4, 4, 3], 3]]
        # @example ?? [1, 2, 3, 3, 4, 4]
        # @example Print[RemoveLast["Hello, World!", "l"]]
        # @example ?? "Hello, Word!"
        # @example Print[RemoveLast["Hello, World!", "ll"]]
        # @example ?? "Hello, World!"
        #>>
        "RemoveLast" => lambda { |inst, list, ent|
            iter = inst.cast_list(list)
            index = iter.rindex ent
            iter.delete_at index unless index.nil?
            reform_list iter, list
        },
        #<<
        # Returns <code>list</code> without any of the elements specified by <code>ents</code>.
        # @reforms
        # @type list [(*)]
        # @type ents [(*)]
        # @return [(*)]
        # @genre list
        # @example Print[RemoveAmong[1:9, 4:7]]
        # @example ?? [1, 2, 3, 8, 9]
        # @example Print[RemoveAmong["Hello, World!", $A:$Z]]
        # @example ?? ello, orld!
        # @example Print[RemoveAmong[12930, 0'2'4'6'8]]
        # @example ?? 12930
        #>>
        "RemoveAmong" => lambda { |inst, list, ents|
            ents = ents.nil? ? [ents] : [*ents]
            iter = inst.cast_list(list)
            iter.reject! { |e| ents.include? e }
            reform_list iter, list
        },
        #<<
        # Returns <code>ent</code> repeated <code>amt</code> times.
        # @type ent (*)
        # @type ent number
        # @return [(*)]
        # @genre list
        # @example Print[Repeat["Hello!", 3]]
        # @example ?? ["Hello!", "Hello!", "Hello!"]
        # @example Print[Repeat[4, 0]]
        # @example ?? []
        # @example Print[Repeat[9, 1:5]]
        # @example ?? [[9], [9, 9], [9, 9, 9], [9, 9, 9, 9], [9, 9, 9, 9, 9]]
        #>>
        "Repeat" => vectorize_dyad(RIGHT) { |inst, ent, amt|
            Array.new(amt) { ent }
        },
        #<<
        # Returns an array representing the run-length encoded version of <code>list</code>.
        # @type list (*)
        # @return [[(*),  number]]
        # @genre list
        # @example Display[RLE["hmmmmm..."]]
        # @example  "h" 1
        # @example  "m" 5
        # @example  "." 3
        #>>
        "RLE" => lambda { |inst, list|
            inst.cast_list(list)
                .chunk { |e| e }
                .map { |k, v| [k, v.size] }
        },
        #<<
        # Rotates <code>list</code> to the left by <code>amount</code>.
        # @type list [(*)]
        # @type amount number
        # @return [(*)]
        # @example Print[Rotate[1:5, 2]]
        # @example ?? [3, 4, 5, 1, 2]
        # @example Print[Rotate["Sorry!", -1]]
        # @example ?? !Sorry
        # @genre list
        #>>
        "Rotate" => vectorize_dyad(RIGHT) { |inst, list, amount=1|
            iter = inst.cast_list list
            res = rotate iter, amount
            reform_list res, list
        },
        #<<
        # Returns all rotations of <code>list</code>.
        # @type list [(*)]
        # @return [(*)]
        # @example Display[Rotations[1:5]]
        # @example  1 2 3 4 5
        # @example  2 3 4 5 1
        # @example  3 4 5 1 2
        # @example  4 5 1 2 3
        # @example  5 1 2 3 4
        # @genre list
        #>>
        "Rotations" => lambda { |inst, list|
            (0...list.size).map { |rot|
                @@functions["Rotate"][inst, list, rot]
            }
        },
        #<<
        # Returns <code>true</code> if every member in each argument is the same.
        # @type args [(*)]
        # @return bool
        # @example Print[Same[1:5]]
        # @example ?? false
        # @example Print[Same[ [1, 1, 1, 1] ]]
        # @example ?? true
        # @example Print[Same[ [1:3, 1:3, 1:3] ]]
        # @example ?? true
        # @genre list
        #>>
        "Same" => lambda { |inst, *args|
            list = args.flatten(1)
            list.all? { |e| e == list[0] }
        },
        "Sample" => vectorize_dyad(RIGHT) { |inst, list, n=nil|
            sample list, n
        },
        "Set" => lambda { |inst, ent, key, val|
            if String === ent
                scope = inst.locals.last
                scope = inst.variables unless scope.has_key? ent
                scope[ent][key] = val
            else
                ent[key] = val
            end
        },
        "Keys" => lambda { |inst, hash|
            hash.keys
        },
        "SetMatrix" => lambda { |inst, mat, row, col, val|
            mat[row][col] = val
        },
        "Size" => lambda { |inst, list|
            if Numeric === list
                list.abs.to_s.size
            elsif class_has? list, "$size"
                list["$size"][inst]
            elsif Proc === list
                list.arity.abs - 1
            elsif AtState.func_like? list
                list.arity rescue list.size - 1 rescue nil
            else
                list.size
            end
        },
        #<<
        # Returns all slices of <code>list</code> of length <code>skew</code>.
        # @type list [(*)]
        # @type skew number|[number]
        # @return [[(*)]]
        # @genre list
        # @example Print[Slices[1:6, 2]]
        # @example ?? [[1, 2], [2, 3], [3, 4], [4, 5], [5, 6]]
        #>>
        "Slices" => lambda { |inst, list, skew=(1..list.size).to_a|
            if skew.is_a? Array
                skew.flat_map { |e|
                    @@functions["Slices"][inst, list, e]
                }
            else
                forced = inst.cast_list(list)
                res = slices forced, skew
                res.map { |e|
                    reform_list e, list
                }
            end
        },
        #<<
        # Returns all slices of <code>list</code> of length <code>skew</code>, including an implied threshold of entities to the left and right of <code>list</code>.
        # @type list [(*)]
        # @type skew number|[number]
        # @return [[(*)]]
        # @genre list
        # @option fill The entity to fill the threshold. Default: <code>0</code>.
        # @option compact If <code>true</code>, removes the treshold after the slices are generated. Default: <code>false</code>.
        # @option repeat
        # @example Print[SlicesFill[1:6, 2]]
        # @example ?? [[0, 1], [1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 0]]
        # @example Print[SlicesFill["a":"f", 2, fill->" "]]
        # @example [[" ", "a"], ["a", "b"], ["b", "c"], ["c", "d"], ["d", "e"], ["e", "f"], ["f", " "]]
        # @example Print[SlicesFill[1:3, 2, compact->true]]
        # @example ?? [[1], [1, 2], [2, 3], [3]]
        # @example Print[SlicesFill[1:3, 2, repeat->3]]
        # @example ?? [[0, 0], [0, 0], [0, 1], [1, 2], [2, 3], [3, 0], [0, 0], [0, 0]]
        #>>
        "SlicesFill" => configurable { |inst, list, skew=(1..list.size).to_a, **opts|

            fill = get_default(opts, :fill, 0)
            compact = get_default(opts, :compact, false)
            repeat = get_default(opts, :repeat, nil)

            list = list.dup

            if compact
                first = last = :none
            elsif AtState.func_like? fill
                first, last = [list.first, list.last].map { |e|
                    fill[inst, e]
                }
            else
                first = last = fill
            end

            res = if skew.is_a? Array
                skew.flat_map { |e|
                    slices_fill list, e, first, last, repeat
                }
            else
                slices_fill list, skew, first, last, repeat
            end
            res.map { |e| e - [:none] }
        },
        "Smaller" => vectorize_dyad { |inst, *args|
            args.min
        },
        "Sort" => lambda { |inst, list, func=nil|
            if String === list
                return @@functions["Sort"][inst, list.chars].join
            end
            if func.nil?
                list.sort
            else
                list.sort { |x, y|
                    func[inst, x, y]
                }
            end
        },
        "Shuffle" => lambda { |inst, list|
            shuffled = inst.cast_list(list).shuffle
            reform_list shuffled, list
        },
        "SortBy" => curry { |inst, func, ent|
            list = inst.cast_list ent
            res = list.sort_by { |e|
                res = func[inst, e]
                if res == !!res
                    res = force_number res
                end
                res.nil? ? Infinity : res
            }
            reform_list res, ent
        },
        "SplitAt" => vectorize_dyad(RIGHT) { |inst, str, inds=[1]|
            split_at(inst.cast_list(str), inds).map { |e|
                reform_list e, str
            }
        },
        #<<
        # Calculates the standard deviation of <code>list</code>.
        # @type list [number]
        # @return number
        # @genre list
        #>>
        "StdDev" => lambda { |inst, list|
            list.stddev
        },
        "Stitch" => lambda { |inst, *args|
            if args.all? { |e| String === e }
                grid = @@functions["Grid"]
                zipwith(*args.map { |e| grid[inst, e] }) { |*args|
                    args.reject(&:nil?).join
                }.join "\n"
            else
                zipwith(*args.map { |e| deep_copy e }) { |*args|
                    args.reject(&:nil?).inject(:concat)
                }
            end
        },
        "Subsets" => lambda { |inst, list, n=list.size, exclude=[]|
            # p list, n, exclude
            if n < 0
                n = (list.size + n) % list.size
            end
            res = [[]]
            exclude = [*exclude]
            res.concat @@functions["Slices"][inst, list, (1..n).to_a]
            res.delete_if { |e| exclude.include? e.size }
        },
        "Sum" => lambda { |inst, list|
            list = inst.cast_list(list)
            head = String === list.first ? "" : 0 rescue 0
            list.inject(head) { |a, e|
                @@operators["+"][inst, a, e]
            }
        },
        "UnSlices" => lambda { |inst, list|
            list[0...-1].map(&:first) + list[-1]
        },
        #<<
        # Treats <code>ent</code> as a hash where keys are indices and values are values, and converts to an array,
        # filling in empty values with <code>fill</code>.
        # @type ent Hash[(*) -> (*)]
        # @type fill (*)
        # @return [(*)]
        # @genre list
        # @param fill default: <code>nil</code>.
        # @example test := <~ 0 -> 3, 2 -> 4, 6 -> 3 ~>
        # @example Print[UnSparse[test]]
        # @example ?? [3, nil, 4, nil, nil, nil, 3]
        # @example Print[UnSparse[test, 0]]
        # @example ?? [3, 0, 4, 0, 0, 0, 3]
        # @optional fill
        #>>
        "UnSparse" => lambda { |inst, ent, fill=nil|
            if Hash === ent
                res = []
                ent.each { |k, v|
                    ind = force_number k
                    while ind >= res.size
                        res << fill
                    end
                    res[ind] = v
                }
                res
            else
                raise AttacheUnimplementedError.new("`UnSparse` is not defined for non-Hash arguments", inst.position)
            end
        },
        #<<
        # Returns an array of lists representing all pairs <code>(x, y)</code> such that <code>a <= y <= b</code> and <code>a <= x < y</code>.
        # @genre list
        # @type a number
        # @type b number
        # @return [[number, number]]
        #>>
        "TriangleRange" => lambda { |inst, a, b|
            pairs = []

            (a..b).each { |y|
                (a...y).each { |x|
                    pairs << [x, y]
                }
            }

            pairs
        },
        #<<
        # Returns an array of lists representing all pairs <code>(x, y)</code> satisfying <code>cond</code> such that <code>a <= y <= b</code> and <code>a <= x < y</code>.
        # @genre list
        # @type a number
        # @type b number
        # @type cond fn
        # @return [[number, number]]
        #>>
        "TriangleSelect" => lambda { |inst, cond, a, b|
            pairs = []

            (a..b).each { |y|
                (a...y).each { |x|
                    pairs << [x, y] if cond[inst, x, y]
                }
            }

            pairs
        },
        #<<
        # Returns an array representing the union of all lists in <code>lists</code>.
        # @type lists [(*)]
        # @return [(*)]
        # @genre list
        #>>
        "Union" => lambda { |inst, *lists|
            lists.inject(&:|)
        },
        #<<
        # Returns the non-duplicated elements in <code>list</code>.
        # @type list [(*)]
        # @type func fn
        # @optional func
        # @return [(*)]
        # @genre list
        # @reforms
        #>>
        "Unique" => lambda { |inst, list, func=nil|
            fl = inst.cast_list list
            unique = if func.nil?
                fl.uniq
            else
                fl.uniq { |e| arg[func, e] }
            end
            reform_list unique, list
        },
        "Variance" => lambda { |inst, list|
            list.variance
        },

        ##------------------##
        ## Matrix Functions ##
        ##------------------##
        "Diagonal" => vectorize_dyad(RIGHT) { |inst, mat, diag=0|
            diagonal mat, diag
        },
        "Dim" => lambda { |inst, mat|
            dim mat
        },
        "Identity" => vectorize_monad { |inst, size|
            Matrix.identity(size).to_a
        },
        #<<
        # Returns the lower triangle of <code>mat</code>, where the remaining values are replaced with zeroes.
        # @type mat [[(*)]]
        # @type strict bool
        # @optional strict
        # @param strict If <code>true</code>, includes the diagonal. Default: <code>true</code>.
        # @return [[(*)]]
        # @example mat := Integers[3, 3] + 1
        # @example Display[mat]
        # @example ??  1 2 3
        # @example ??  4 5 6
        # @example ??  7 8 9
        # @example Display[LowerTriangle[mat]]
        # @example ??  1 0 0
        # @example ??  4 5 0
        # @example ??  7 8 9
        # @example Display[LowerTriangle[mat, true]]
        # @example ??  0 0 0
        # @example ??  4 0 0
        # @example ??  7 8 0
        # @genre list/matrix
        #>>
        "LowerTriangle" => lambda { |inst, mat, strict=false|
            lower_triangle mat, AtState.truthy?(strict)
        },
        "MatrixRotate" => vectorize_dyad(RIGHT) { |inst, mat, n=1|
            n.times {
                mat = mat.transpose.map(&:reverse)
            }
            mat
        },
        "MatrixIota" => lambda { |inst, mat, fn=nil|
            res = matrix_iota mat
            if fn
                res.map { |row|
                    row.map { |(i, j)|
                        fn[inst, mat[i][j], i, j]
                    }
                }
            else
                res
            end
        },
        "Moore" => configurable { |inst, list, fn, r=1, **opts|
            # see also: http://mathworld.wolfram.com/MooreNeighborhood.html
            cycle = get_default opts, :cycle, false
            list.map.with_index { |row, i|
                row.map.with_index { |c, j|
                    res = (i-r..i+r).map { |y|
                        next unless 0 <= y && y < list.size || cycle
                        (j-r..j+r).map { |x|
                            cond = (i - y).abs <= r && (j - x).abs <= r
                            unless cycle
                                cond &&= 0 <= x && x < row.size
                            end
                            next unless cond
                            from = list[y % list.size]
                            from[x % from.size]
                        }.compact
                    }.compact
                    ar = fn.arity rescue fn.size rescue 1
                    ar > 1 ? fn[inst, res, c] : fn[inst, res] rescue fn[inst, res]
                }
            }
        },
        "MooreN" => configurable { |inst, list, widths=NOT_PROVIDED, r=1, **opts|
            widths = default_sentinel(widths) { (0..dim(list).prod).to_a }
            moores = []
            @@functions["Moore"][inst, list, lambda { |inst, mat|
                moores << mat
            }, r, **opts]
            if Array === widths
                moores.select.with_index { |e, i|
                    widths.include? i
                }
            else
                moores[widths]
            end
        },
        "VonNeumann" => configurable { |inst, list, fn, r=1, **opts|
            # see also: http://mathworld.wolfram.com/vonNeumannNeighborhood.html
            cycle = get_default opts, :cycle, false
            list.map.with_index { |row, i|
                row.map.with_index { |c, j|
                    res = (i-r..i+r).map { |y|
                        next unless 0 <= y && y < list.size
                        (j-r..j+r).map { |x|
                            cond = (i - y).abs + (j - x).abs <= r
                            unless cycle
                                cond &&= 0 <= x && x < list.size
                            end
                            next unless cond
                            list[y][x]
                        }.compact
                    }.compact
                    fn[inst, res]
                }
            }
        },
        "VonNeumannN" => configurable { |inst, list, widths=NOT_PROVIDED, r=1, **opts|
            widths = default_sentinel(widths) { (0..dim(list).prod).to_a }
            von_neumanns = []
            @@functions["VonNeumann"][inst, list, lambda { |inst, mat|
                moores << mat
            }, r, **opts]
            if Array === widths
                von_neumanns.select.with_index { |e, i|
                    widths.include? i
                }
            else
                von_neumanns[widths]
            end
        },
        "Tr" => lambda { |inst, list|
            list.transpose
        },
        "Transpose" => lambda { |inst, list|
            list.transpose
        },
        #<<
        # Returns the upper triangle of <code>mat</code>, where the remaining values are replaced with zeroes.
        # @type mat [[(*)]]
        # @type strict bool
        # @optional strict
        # @param strict If <code>true</code>, includes the diagonal. Default: <code>true</code>.
        # @return [[(*)]]
        # @example mat := Integers[3, 3] + 1
        # @example Display[mat]
        # @example ??  1 2 3
        # @example ??  4 5 6
        # @example ??  7 8 9
        # @example Display[UpperTriangle[mat]]
        # @example ??  1 2 3
        # @example ??  0 5 6
        # @example ??  0 0 9
        # @example Display[UpperTriangle[mat, true]]
        # @example ??  0 2 3
        # @example ??  0 0 6
        # @example ??  0 0 0
        # @genre list/matrix
        #>>
        "UpperTriangle" => lambda { |inst, mat, strict=false|
            upper_triangle mat, AtState.truthy?(strict)
        },

        ##------------------------##
        ## Combinatoric Functions ##
        ##------------------------##
        "Combinations" => lambda { |inst, list, count=NOT_PROVIDED|
            count = default_sentinel(count) {
                (0..inst.cast_list(list).size).to_a
            }
            if count.is_a? Array
                count.map { |e|
                    @@functions["Combinations"][inst, list, e]
                }.flatten(1)
            else
                inner = inst.cast_list list
                inner.combination(count).map { |e|
                    reform_list e, list
                }
            end
        },
        "Permutations" => vectorize_dyad(RIGHT) { |inst, list, count=NOT_PROVIDED|
            listified = inst.cast_list(list)
            size = listified.size
            count = default_sentinel(count, size)
            listified.permutation(count).map { |perm|
                reform_list perm, list
            }.to_a
        },
        "Zip" => lambda { |inst, a, *b|
            inst.cast_list(a).zip(*b.map { |e| inst.cast_list e })
        },
        "ZipWith" => lambda { |inst, fn, a=nil, b=nil|
            l = lambda { |inst, a, b|
                zipwith(inst.cast_list(a), inst.cast_list(b)) { |x, y| fn[inst, x, y] }
            }
            if a.nil?
                l
            else
                l[inst, a, b]
            end
        },

        ##---------------------------##
        ## List Functional Functions ##
        ##---------------------------##
        #<<
        # Folds the function <code>f</code> over the list <code>list</code>, optionally starting with <code>start</code>.
        # @optional start
        # @type f fn
        # @type list [(*)]
        # @type start (*)
        # @return (*)
        # @example Print[Fold[${ x + y }, [2, 3, 6, 7]]]
        # @example ?? 18
        # @example Print[Fold[${ $"f[${x}, ${y}]" }, 1:5]]
        # @example ?? f[f[f[f[1, 2], 3], 4], 5]
        # @example Print[Fold[Add, 123]]
        # @example ?? 6
        # @example Print[Fold[${ y + x }, "Hello, World!"]]
        # @example ?? !dlroW ,olleH
        # @example Print[Fold[Add, [1]]]
        # @example ?? 1
        # @example Display[Fold[Add, []]]
        # @example ?? nil
        # @example Display[Fold[Add, [], 0]]
        # @example ?? 0
        # @genre functional/list
        #>>
        "Fold" => curry(2) { |inst, f, list, start=NOT_PROVIDED|
            @@functions["FoldList"][inst, f, list, start].last
        },
        #<<
        # Cumulatively folds the function <code>f</code> over <code>ent</code>, optionally starting at <code>start</code>.
        # @optional start
        # @type f fn
        # @type ent [(*)]
        # @type start (*)
        # @return [(*)]
        # @genre functional/list
        #>>
        "FoldList" => curry(2) { |inst, f, ent, start=NOT_PROVIDED|
            list = inst.cast_list(ent)
            start = default_sentinel(start) { list.shift }
            results = [start]
            list.each { |e|
                results << f[inst, results.last, e]
            }
            results
        },
        "Map" => vectorize_dyad(LEFT) { |inst, f, list=nil|
            if AtState.func_like? list
                g = list
                lambda { |inst, *args|
                    g[inst, *args].map { |e|
                        f[inst, e]
                    }
                }
            elsif list.nil?
                lambda { |inst, list|
                    @@functions["Map"][inst, f, list]
                }
            elsif class_has? list, "$map"
                map_vector(inst, list) { |inst, e|
                    f[inst, e]
                }
            else
                list.map { |e| f[inst, e] }
            end
        },
        "MapArgs" => lambda { |inst, f, list, *args|
            if AtState.func_like? list
                g = list
                n = args[0] || 1
                lambda { |inst, list, *args|
                    g[inst, list].map { |e|
                        f[inst, e, *resize(args + [list, e], n)]
                    }
                }
            else
                list.map { |e| f[inst, e, *args] }
            end
        },
        "MaxBy" => curry { |inst, f, list|
            list.max_by { |e| f[inst, e] }
        },
        "MinBy" => curry { |inst, f, list|
            list.min_by { |e| f[inst, e] }
        },
        # note: at *least* 2 arguments
        "Outer" => curry(2) { |inst, f, *lists|
            if lists.empty?
                lambda { |inst, *lists|
                    combine(*lists) { |*e|
                        f[inst, *e]
                    }
                }
            else
                combine(*lists) { |*e|
                    f[inst, *e]
                }
            end
        },
        # note: at *least* 2 arguments
        "Cross" => curry(2) { |inst, func, *lists|
            head, *rest = lists
            head.product(*rest).map { |e|
                func[inst, *e]
            }
        },
        "Select" => curry { |inst, f, list|
            iter = inst.cast_list list
            res = iter.select { |e|
                AtState.truthy? f[inst, e]
            }
            reform_list res, list
        },
        "Table" => curry(2) { |inst, f, as, bs=as|
            as.map { |a|
                bs.map { |b|
                    f[inst, a, b]
                }
            }
        },
        "Reject" => lambda { |inst, f, list|
            if AtState.func_like? list
                g = list
                lambda { |inst, *args|
                    @@functions["Reject"][inst, f, g[inst, *args]]
                }
            else
                list.reject { |e| AtState.truthy? f[inst, e] }
            end
        },


        ##########################
        #### STRING FUNCTIONS ####
        ##########################
        #<<
        # Returns the characters of <code>str</code>.
        # @return [string]
        # @type str string
        # @genre string
        #>>
        "Chars" => vectorize_monad { |inst, str|
            str.chars
        },
        #<<
        # Selects the first key (or value) in <code>opts</code> which start with <code>val</code>.
        # @genre string
        # @return string
        # @type opts [string]|{string->(*)}
        # @type val string
        # @example options := <~
        # @example   stop -> 0,
        # @example   continue -> 1,
        # @example   redo -> 2
        # @example ~>
        # @example choice := "s"
        # @example Print[FindHead[options, choice]]
        # @example ?? stop
        #>>
        "FindHead" => lambda { |inst, opts, val|
            if Hash === opts
                opts = opts.keys
            end

            opts.sort.select { |e|
                e.start_with? val
            }.first
        },
        #<<
        # Formats a string. Currently, uses Ruby's <a href="https://ruby-doc.org/core-2.0.0/String.html#method-i-25"><code>%</code></a> method on strings.
        # @type str string
        # @type args (*)
        # @return string
        # @genre string
        # @example Print[Format["%s is a %s", "Java", "Joke"]]
        # @example ?? Java is a Joke
        #>>
        "Format" => lambda { |inst, str, *args|
            str % args
        },
        #<<
        # Returns the characters of each line of <code>str</code>.
        # @type str string
        # @type inner string
        # @return string
        # @optional inner
        # @param inner Character to pad the ends of each line with. Default: <code>" "</code>.
        # @genre string
        # @example Map[Print, Grid["Hello,\nWorld\nof mine!"]]
        # @example ?? ["H", "e", "l", "l", "o", ",", " ", " "]
        # @example ?? ["W", "o", "r", "l", "d", " ", " ", " "]
        # @example ?? ["o", "f", " ", "m", "i", "n", "e", "!"]
        #>>
        "Grid" => lambda { |inst, str, inner=" "|
            str = str.lines rescue str
            grid = str.map(&:chomp).map(&:chars) rescue str
            pad_grid grid, inner
        },
        #<<
        # Joins <code>list</code> by <code>joiner</code>.
        # @type list [(*)]
        # @type joiner string
        # @optional joiner
        # @return string
        # @genre string
        #>>
        "Join" => vectorize_dyad(RIGHT) { |inst, list, joiner=""|
            list.join joiner
        },
        #<<
        # Matches first occurrence of <code>match</code> (cast to regex) in <code>source</code>.
        # If only one argument <code>source</code> is given, then returns a function which takes the source as input.
        # @type source string
        # @type match (*)
        # @return string
        # @genre string/regex
        # @example Print[Match["no you?", ".{3}"]]
        # @example ?? "no "
        # @example Print[Match["cat in the hat", "[ch]at"]]
        # @example ?? "cat"
        # @example match2vowels := Match[`"\w*[aeiou]{2}\w*"]
        # @example Print[match2vowels["Hello, loopy man!"]]
        # @example ?? "loopy"
        #>>
        "Match" => vectorize_dyad { |inst, source, match|
            if match.nil?
                match = source
                lambda { |inst, source|
                    @@functions["Match"][inst, source, match]
                }
            else
                match = make_regex match
                res = source.match(match).to_a
                if res.empty?
                    nil
                elsif res.size == 1
                    res[0]
                else
                    res
                end
            end
        },
        #<<
        # Matches all occurrences of <code>match</code> in <code>source</code>.
        # @type source string
        # @type match (*)
        # @return [(*)]
        # @genre string/regex
        # @example Print[MatchAll["no you?", ".{3}"]]
        # @example ?? ["no ", "you"]
        # @example Print[MatchAll["cat in the hat", "[ch]at"]]
        # @example ?? ["cat", "hat"]
        # @example match2vowels := MatchAll[`"\w*[aeiou]{2}\w*"]
        # @example Print[match2vowels["Hello, loopy man!"]]
        # @example ?? ["loopy"]
        #>>
        "MatchAll" => vectorize_dyad { |inst, source, match|
            if match.nil?
                match = source
                lambda { |inst, source|
                    @@functions["MatchAll"][inst, source, match]
                }
            else
                match = make_regex match
                source.scan(match)
            end
        },
        #<<
        # Returns the UTF-8 ordinal representing <code>ent</code>.
        # @type ent string|number
        # @optional offset
        # @type offset number
        # @param offset Specifies the offset at which the ordinal is taken.
        # @return number
        # @genre string
        # @example Print[Ord["A"]]
        # @example ?? 65
        # @example Print[Ord["asdf"]]
        # @example ?? 97
        # @example Print[Ord["asdf", 1]]
        # @example ?? 115
        # @example Print[Ord["asdf", 1:3]]
        # @example ?? [115, 100, 102]
        # @example Print[Ord[33]]
        # @example ?? 33
        #>>
        "Ord" => vectorize_dyad { |inst, ent, offset=0|
            Numeric === ent ? ent : ent[offset].ord
        },
        #<<
        # Maps <code>Ord</code> over the characters of <code>ent</code> if it is a string;
        # otherwise, is equivalent to <code>[Ord[ent]]</code>.
        # @type ent string|number
        # @return [number]
        # @genre string
        # @example Print[Ords["Hello!"]]
        # @example ?? [72, 101, 108, 108, 111, 33]
        # @example Print[Ords[ [33, 34, 35] ]]
        # @example ?? [32, 34, 35]
        #>>
        "Ords" => vectorize_monad { |inst, ent|
            if ent.is_a? String
                ent.chars.map(&:ord)
            else
                ent.ord
            end
        },
        #<<
        # Pads <code>ent</code> to be no less than length <code>amt</code>, padding with fill elements <code>fill</code> on the left.
        # @type ent (*)
        # @type amt number
        # @type fill (*)
        # @return (*)
        # @genre string
        # @optional fill
        # @paramtype string ent appends space characters to the left of <code>ent</code>.
        # @paramtype [(*)] ent appends <code>0</code>s to the left of <code>ent</code>.
        # @example Display[PadLeft["Charles", 20]]
        # @example ?? "             Charles"
        # @example Print[PadLeft[1:3, 6]]
        # @example ?? [0, 0, 0, 1, 2, 3]
        #>>
        "PadLeft" => lambda { |inst, ent, amt, fill=NOT_PROVIDED|
            case ent
                when String
                    fill = default_sentinel(fill, " ")
                    ent.rjust(amt, fill)

                else
                    fill = default_sentinel(fill, 0)
                    ent = inst.cast_list ent
                    until ent.size >= amt
                        ent = [fill, *ent]
                    end
                    ent
            end
        },
        #<<
        # Pads <code>ent</code> to be no less than length <code>amt</code>, padding with fill elements <code>fill</code> on the right.
        # @type ent (*)
        # @type amt number
        # @type fill (*)
        # @return (*)
        # @genre string
        # @optional fill
        # @paramtype string ent appends space characters to the right of <code>ent</code>.
        # @paramtype [(*)] ent appends <code>0</code>s to the right of <code>ent</code>.
        # @example Display[PadRight["Charles", 20]]
        # @example ?? "Charles             "
        # @example Print[PadRight[1:3, 6]]
        # @example ?? [1, 2, 3, 0, 0, 0]
        #>>
        "PadRight" => lambda { |inst, ent, amt, fill=NOT_PROVIDED|
            case ent
                when String
                    fill = default_sentinel(fill, " ")
                    ent.ljust(amt, fill)

                else
                    fill = default_sentinel(fill, 0)
                    ent = inst.cast_list ent
                    until ent.size >= amt
                        ent = [*ent, fill]
                    end
                    ent
            end
        },
        #<<
        # Centers <code>ent</code> to <code>amt</code> elements, padding with
        # <code>pad</code>.
        # @type ent [(*)]
        # @type amt number
        # @type pad (*)
        # @optional pad
        # @return string
        # @param pad Default: <code>" "</code> or <code>0</code>, depending on <code>ent</code>.
        # @example Print[Center["a", 3]]
        # @example ?? " a "
        # @example Print[Center[[1, 2], 6]]
        # @example ?? [0, 0, 1, 2, 0, 0]
        # @example Print[Center["Hi!", 10, "~"]]
        # @example "~~~Hi!~~~~"
        # @example Print[Center[9, 5, 1]]
        # @example ?? 11911
        # @genre string
        #>>
        "Center" => lambda { |inst, ent, amt, pad=NOT_PROVIDED|
            list = inst.cast_list(ent)
            deficit = amt - list.size
            return reform_list(list, ent) if deficit <= 0

            pad = default_sentinel(pad) {
                inst.default_cell_type(list[0]) rescue 0
            }

            before = deficit / 2
            after = deficit - before

            result = Array.new(before) { pad } + list + Array.new(after) { pad }

            reform_list(result, ent)
        },
        #<<
        # Splits <code>str</code> on occurrences of <code>sep</code>. When
        # <code>sep</code> is omitted, splits <code>str</code> on whitespace.
        # @type str string
        # @type sep string|regex
        # @optional sep
        # @return [string]
        # @genre string
        #>>
        "Split" => vectorize_dyad { |inst, str, sep=/\s+/|
            str.split sep
        },
        #<<
        # Replaces each character in <code>source</code> with the respective
        # character in <code>repl</code> in <code>str</code>. Character ranges
        # are specified with <code>-</code>.
        # @type str string
        # @type source string
        # @type repl string
        # @return string
        # @genre string
        # @example Print[Translate["Hello, World!", "a-z", "A-Z"]]
        # @example ?? "HELLO, WORLD!"
        # @example Print[Translate["No one is here today, James", "a-zA-Z", "A-Za-z"]]
        # @example ?? "nO ONE IS HERE TODAY, jAMES"
        #>>
        "Translate" => lambda { |inst, str, source, repl|
            str.tr source, repl
        },
        #<<
        # Returns all lines of <code>str</code>; splits on line breaks, which
        # are newlines preceeded by an optional carriage reutrn.
        # @type str string
        # @return [string]
        # @genre string
        # @example Print[Lines["abc\nhello"]]
        # @example ?? ["abc", "hello"]
        #>>
        "Lines" => vectorize_monad { |inst, str|
            str.split(/\r?\n/)
        },
        #<<
        # Replaces all occurrences of <code>search</code> in <code>str</code> with <code>replace</code>.
        # @type str string
        # @type search string|regex
        # @type replace string
        # @return string
        # @optional replace
        # @param Default: <code>""</code>.
        # @genre string
        #>>
        "Replace" => lambda { |inst, str, search, replace=""|
            replace str, search, replace
        },
        #<<
        # For each ConfigureValue <code>search -&gt; replace</code> in <code>args</code>, replaces all occurrences of <code>search</code> in <code>str</code> with <code>replace</code>. Works iteratively, starting with the first replacement.
        # @type str string
        # @type args ConfigureValue
        # @return string
        # @genre string
        # @example Print[ReplaceMultiple[
        # @example     "Hello, World!",
        # @example     ll -> "no",
        # @example     oo -> "",
        # @example     l -> "q"
        # @example ]]
        # @example ?? Hen, Worqd!
        #>>
        "ReplaceMultiple" => lambda { |inst, str, *args|
            str = str.dup
            args.map(&:to_a).each { |k, v|
                str.gsub!(Regexp.new(k), v)
            }
            str
        },
        #<<
        # For each <code>e</code> matching <code>search</code> found in <code>str</code>, replaces it with <code>func[e]</code>.
        # @type str string
        # @type search string|regex
        # @type func fn
        # @return string
        # @genre string
        # @example Print[ReplaceF["Student 9234-B hsa received a 97.03% on their test.", /"\\d+", Reverse]]
        # @example ?? Student 4329-B hsa received a 79.30% on their test.
        #>>
        "ReplaceF" => lambda { |inst, str, search, func|
            str.gsub(search) { |e|
                func[inst, e]
            }
        },
        "Repr" => lambda { |inst, ent|
            ent.inspect
        },
        #<<
        # Translates every alphabetic character in <code>str</code> <code>amount</code> character to the right, wrapping around.
        # @type str string
        # @type amount number
        # @return str
        # @example Print[Rot["Hello, World!"]]
        # @example ?? Uryyb, Jbeyq!
        # @example Print[Rot["Hello, World!", 1]]
        # @example ?? Ifmmp, Xpsme!
        # @example Print[Rot["Ifmmp, Xpsme!", -1]]
        # @example ?? Hello, World!
        # @genre string
        #>>
        "Rot" => vectorize_dyad(RIGHT) { |inst, str, amount=13|
            rotN(str, amount)
        },
        #<<
        # Converts <code>ent</code> to a string.
        # @type ent (*)
        # @return string
        # @genre conversion
        #>>
        "String" => lambda { |inst, ent|
            inst.cast_string ent
        },
        "Strip" => lambda { |inst, str|
            str.strip
        },
        #<<
        # Removes <code>to_chomp</code> once from the end of <code>ent</code>.
        # @type ent (*)
        # @type to_chomp (*)
        # @return (*)
        # @genre string
        # @param to_chomp For strings, defaults to <code>"\n"</code>; otherwise, <code>0</code>.
        #>>
        "Chomp" => lambda { |inst, ent, to_chomp=NOT_PROVIDED|
            if String === ent
                to_chomp = default_sentinel to_chomp, "\n"
                ent.chomp to_chomp
            else
                to_chomp = default_sentinel to_chomp, 0
                list = inst.cast_list ent
                if !list.empty? && list.last == to_chomp
                    list.pop
                end
                reform_list(list, ent)
            end
        },
        "SwapCase" => vectorize_monad { |inst, str|
            str.chars.map { |e|
                if e != e.upcase
                    e.upcase
                elsif e != e.downcase
                    e.downcase
                else
                    e
                end
            }.join
        },
        #<<
        # Replaces all lowercase letters in <code>str</code> with the respective uppercase one.
        # @type str string
        # @return string
        # @example Print[Upcase["Hello, World!"]]
        # @example ?? HELLO, WORLD!
        # @example Print[Upcase[">>> NO ONE"]]
        # @example ?? >>> NO ONE
        # @example Print[Upcase["le monde"]]
        # @example ?? LE MONDE
        # @genre string
        #>>
        "Upcase" => vectorize_monad { |inst, str|
            str.upcase
        },
        "UnGrid" => lambda { |inst, str|
            str.map(&:join).join "\n"
        },
        #<<
        # Replaces all uppercase letters in <code>str</code> with the respective lowercase one.
        # @type str string
        # @return string
        # @example Print[Downcase["Hello, World!"]]
        # @example ?? hello, world!
        # @example Print[Downcase[">>> NO ONE"]]
        # @example ?? >>> no one
        # @example Print[Downcase["le monde"]]
        # @example ?? le monde
        # @genre string
        #>>
        "Downcase" => vectorize_monad { |inst, str|
            str.downcase
        },
        "IsUpcase" => vectorize_monad { |inst, str|
            str.upcase == str
        },
        "IsDowncase" => vectorize_monad { |inst, str|
            str.downcase == str
        },
        "IsAlpha" => vectorize_monad { |inst, str|
            /^[[:alpha:]]*$/ === str.to_s
        },
        "IsAlphaNumeric" => vectorize_monad { |inst, str|
            /^[[:alnum:]]*$/ === str.to_s
        },
        "IsNumeric" => vectorize_monad { |inst, str|
            /^[[:digit:]]*$/ === str.to_s
        },


        ########################
        #### DATE FUNCTIONS ####
        ########################
        "Date" => lambda { |inst, *args|
            Time.new *args
        },
        "DateFormat" => lambda { |inst, fmt="%B %-d, %Y", date=Time.now|
            date.strftime fmt
        },
        "DayOfYear" => vectorize_dyad { |inst, n, date=Time.now|
            if date.is_a? Numeric
                date = Time.new date
            end
            date.day_of_year n
        },
        "YearDays" => lambda { |inst, date=Time.now|
            res = []
            dates = date.is_a?(Array) ? date : [date]
            dates.each { |date|
                res.concat yearlike(date).year_days
            }
            res
        },
        "DayOfWeek" => vectorize_monad { |inst, date|
            date.week_day
        },
        "Weekday" => vectorize_monad { |inst, date|
            date.wday
        },
        "Day" => vectorize_monad { |inst, date=Time.now|
            date.day
        },

        ##################
        #### UNSORTED ####
        ##################
        "Debug_" => lambda { |inst, arg|
            di "debugging function instance"
            p arg.raw
            p arg
            dd "end debugging"

        },
        "BigDecimal" => vectorize_monad { |inst, n|
            require 'bigdecimal'
            BigDecimal.new n
        },
        "Error" => lambda { |inst, name, msg="An error has occured."|
            AtError.new name, msg
        },
        "EscapeRegex" => lambda { |inst, str|
            Regexp.escape str.to_s
        },
        #<<
        # Escapes <code>str</code> such that it is safe to be passed to an HTML document. (Escapes the following: <code>&<>"</code>.)
        # @type str string
        # @return string
        # @genre string/HTML
        #>>
        "HTMLEscape" => lambda { |inst, str|
            str.gsub("&", "&amp;").gsub("<", "&lt;").gsub(">", "&gt;").gsub("\"", "&quot;")
        },
        "DoSafe" => lambda { |inst, body|
            begin
                inst.evaluate_node body
                nil
            rescue Exception => e
                e
            end
        },
        "TryCatch" => lambda { |inst, body, catch|
            res = inst.evaluate_node body
            if AtError === res
                inst.evaluate_node catch, [res], check_error: false
                nil
            else
                res
            end
        },
        "Safely" => lambda { |inst, func, catch=nil, **opts|
            rec = lambda { |inst, *args|
                begin
                    func[inst, *args]
                rescue Exception => e
                    if catch.nil?
                        e
                    elsif AtState.func_like? catch
                        catch[inst, e]
                    else
                        catch
                    end
                    if opts[:redo]
                        rec[inst, *args]
                    end
                end
            }
        },
        "Commonest" => lambda { |inst, ent, n=1|
            commonest = list.each_with_object(Hash.new(0)) { |m, h|
                h[m] += 1
            }
            .group_by(&:last)
            .sort_by(&:last)
            .reverse_each { |k, v|
                v.map &:first
            }
            if Array === n
                n.map { |i| commonest[i > 0 ? i - 1 : i] }
            else
                commonest[n > 0 ? n - 1 : n]
            end
        },
    }

    # operators with two arguments
    @@operators = {
        #<<
        # Obtains the member in <code>obj</code> with <code>prop</code> as a key.
        # </p><p><em>Note:</em> this feature is not completely finished, and may have unintended behaviours, such as <code>[1, 2, 3].size</code>.
        # @type prop raw
        # @type obj (*)
        # @return (*)
        # @genre operator
        # @example ages := <~
        # @example   john -> 32,
        # @example   bob -> 14,
        # @example   dude -> 92
        # @example ~>
        # @example Print[ages.john]
        # @example ?? 32
        # @example
        # @example Person := Class! {
        # @example     name .= _1
        # @example     age .= _2
        # @example }
        # @example john := New[Person, "John Smith", 43]
        # @example Print[john.name, john.age, joiner->", "]
        # @examlpe ?? John Smith, 43
        #>>
        "." => lambda { |inst, obj, prop|
            if Node === prop
                raise AttacheValueError.new("expected simple property instead of a Node", prop.head.position)
            end

            if AtClassInstance === obj || Hash === obj
                obj[prop.raw]
            elsif obj.respond_to? prop.raw.to_sym
                obj.send prop.raw.to_sym
            else
                raise AttacheValueError.new(
                    "`.` expected object to have properties",
                    inst.position
                )
            end
        },
        ":=" => lambda { |inst, var, val|
            if Node === var
                #todo: pattern matching++

                args = var.children.map { |e|
                    if Node === e
                        e.children[0].raw
                        STDERR.puts "Note: undefined behaviour raised: unimplemented argument matching"
                    else
                        e.raw
                    end
                }
                if var.head.raw == "'"
                    val = inst.evaluate_node val
                    args.each_with_index { |arg, i|
                        inst.define arg, val[i]
                    }
                else
                    res = AtLambda.new [val], args
                    if var.head.type == :op_quote
                        inst.set_op_quote var.head, res
                    else
                        inst.define var.head.raw, res
                    end
                end
            else
                res = inst.evaluate_node(val)
                if var.type == :op_quote
                    unless AtState.func_like? res
                        res = lambda { |*discard| res }
                    end
                    inst.set_op_quote var, res
                else
                    name = var.raw
                end
                inst.define name, res
            end
        },
        "≔" => held(true, true) { |inst, var, val|
            @@operators[":="][inst, var, val]
        },
        # only overwrite if nil/undefined
        "::=" => held(true, true) { |inst, var, val|
            if Token === var
                update = begin
                    res = get_variable var.raw
                    res.nil?
                rescue
                    true
                end
                @@operators[":="][inst, var, val] if update
            else
                raise AttacheUnimplementedError.new("::= is not defined for non-token first argument", inst.position)
            end
        },
        ".=" => lambda { |inst, var, val|
            #todo: expand like :=
            #todo: abstract `.=` and `:=` logic
            if Node === var
                args = var.children.map(&:raw)
                if var.head.raw == "'"
                    args.each_with_index { |arg, i|
                        inst.define_local arg, val[i]
                    }
                else
                    res = AtLambda.new [val], args
                    inst.define_local var.head.raw, res
                end
            else
                name = var.raw
                inst.define_local name, inst.evaluate_node(val)
            end
        },
        # only overwrite if nil/undefined
        "..=" => held(true, true) { |inst, var, val|
            if Token === var
                update = begin
                    res = inst.get_variable var.raw
                    res.nil?
                rescue
                    true
                end
                @@operators[".="][inst, var, val] if update
            else
                raise AttacheUnimplementedError.new("::= is not defined for non-token first argument", inst.position)
            end
        },
        "@=" => held(true) { |inst, left, value|
            ent = inst.get_variable left.head.raw
            inds = left.children.map { |e|
                inst.evaluate_node e
            }
            until inds.size == 1
                ent = ent[inds.shift]
            end
            ent[inds.first] = value
        },
        #<<
        # Multiplication.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "*" => vectorize_dyad { |inst, a, b|
            if class_has? a, "$mul"
                a["$mul"][inst, b]
            elsif class_has? b, "$rmul"
                b["$rmul"][inst, a]
            elsif String === b
                b * a
            else
                a * b
            end
        },
        #<<
        # Divides <code>a</code> by <code>b</code>. If <code>a / b</code> represents an integer, the argument becomes an integer. E.g., <code>4 / 2</code> is <code>2</code>, not <code>2.0</code>.
        # @type a number
        # @type b number
        # @return number
        # @genre operator
        #>>
        "/" => vectorize_dyad { |inst, a, b|
            # p 'div',a,b
            if class_has? a, "$div"
                a["$div"][inst, b]
            elsif class_has? b, "$rdiv"
                b["$rdiv"][inst, a]
            elsif AtState.func_like? a
                AtFunction.from(arity: b) { |inst, *args|
                    a[inst, *args]
                }
            else
                simplify_number a * 1.0 / b
            end
        },
        #<<
        # Creates a fraction <code>a / b</code>. See also: <code><a href="#Rational">Rational</a></code>
        # @type a number|fn
        # @type b number
        # @return rational
        # @paramtype fn a Returns a function which, given <code>e</code>, returns <code>Nest[a, e, b]</code>.
        # @genre operator
        #>>
        "//" => vectorize_dyad { |inst, a, b|
            if AtState.func_like? a
                lambda { |inst, e|
                    @@functions["Nest"][inst, a, e, b]
                }
            else
                Rational(a, b)
            end
        },
        #<<
        # Creates a fraction <code>a / b</code>. See also: <code><a href="#Rational">Rational</a></code> and <code><a href="#//">//</a></code>.
        # @type a number
        # @type b number
        # @return rational
        # @genre operator
        #>>
        "⁄" => vectorize_dyad { |inst, a, b|
            Rational(a, b)
        },
        #<<
        # Subtraction.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "-" => vectorize_dyad { |inst, a, b|
            if class_has? a, "$sub"
                a["$sub"][inst, b]
            elsif class_has? b, "$rsub"
                b["$rsub"][inst, a]
            else
                a - b
            end
        },
        #<<
        # Addition.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "+" => vectorize_dyad { |inst, a, b|
            if class_has? a, "$add"
                a["$add"][inst, b]
            elsif class_has? b, "$radd"
                b["$radd"][inst, a]
            else
                a + b
            end
        },
        #<<
        # Returns <code><a href="#PlusMinus">PlusMinus</a>[a, b]</a></code>.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "±" => vectorize_dyad { |inst, a, b|
            @@functions["PlusMinus"][inst, a, b]
        },
        #<<
        # Exponentiation.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "^" => vectorize_dyad { |inst, a, b|
            if class_has? a, "$pow"
                a["$pow"][inst, b]
            elsif class_has? b, "$rpow"
                b["$rpow"][inst, a]
            else
                a ** b
            end
        },
        #<<
        # Modulo.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator
        #>>
        "%" => vectorize_dyad { |inst, a, b|
            if class_has? a, "$mod"
                a["$mod"][inst, b]
            elsif class_has? b, "$rmod"
                b["$rmod"][inst, a]
            else
                a % b
            end
        },
        #<<
        # Returns <code>true</code> if <code>a</code> divides evenly into <code>b</code>.
        # @type a (*)
        # @type b fn|(*)
        # @return (*)
        # @genre operator/logic
        # @paramtype fn b When <code>b</code> a function, calls <code>b[a]</code>.
        #>>
        "|" => vectorize_dyad { |inst, a, b|
            if AtState.func_like? b
                b[inst, a]
            else
                @@operators["%"][inst, b, a] == 0
            end
        },
        #<<
        # Calls <code>y</code> with single parameter <code>x</code>.
        # @type x (*)
        # @type y fn
        # @return (*)
        # @genre operator
        #>>
        "|>" => lambda { |inst, x, y|
            y[inst, x]
        },
        #<<
        # Calls <code>y</code> with single parameter <code>x</code>. See also: <a href="#|>"><code>|&gt;</code></a>.
        # @type x (*)
        # @type y fn
        # @return (*)
        # @genre operator
        #>>
        "▷" => lambda { |inst, x, y|
            @@operators["|>"][inst, x, y]
        },
        #<<
        # Calls <code>x</code> with single parameter <code>y</code>.
        # @type x fn
        # @type y (*)
        # @return (*)
        # @genre operator
        #>>
        "<|" => lambda { |inst, x, y|
            x[inst, y]
        },
        #<<
        # Calls <code>x</code> with single parameter <code>y</code>. See also: <a href="#<|"><code>&lt;|</code></a>.
        # @type x fn
        # @type y (*)
        # @return (*)
        # @genre operator
        #>>
        "◁" => lambda { |inst, x, y|
            @@operators["<|"][inst, x, y]
        },
        #<<
        # Returns <code>true</code> if <code>x</code> equals <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "=" => vectorize_dyad { |inst, x, y|
            x == y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> does <em>not</em> equal <code>y</code>, <code>false</code> otherwise.
        # <em>Note:</em> does not call operator equality.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "/=" => vectorize_dyad { |inst, x, y|
            x != y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> does <em>not</em> equal <code>y</code>, <code>false</code> otherwise. See also: <a href="#/="><code>/=</code></a>.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "≠" => vectorize_dyad { |inst, x, y|
            x != y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> equals <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "==" => lambda { |inst, x, y|
            if class_has? x, "$equal"
                x["$equal"][inst, y]
            elsif class_has? y, "$equal"
                y["$equal"][inst, x]
            elsif Type === x
                x.to_s == y
            elsif Type === y
                x == y.to_s
            else
                x == y
            end
        },
        #<<
        # Synonym for <a href="#=="><code>==</code></a>.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "is" => lambda { |inst, x, y|
            @@operators["=="][inst, x, y]
        },
        #<<
        # Returns <code>true</code> if <code>x</code> does <em>not</em> equal <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "=/=" => lambda { |inst, x, y|
            x != y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is greater than <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        ">" => vectorize_dyad { |inst, x, y|
            x > y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is less than <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "<" => vectorize_dyad { |inst, x, y|
            x < y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is greater than or equal to <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        ">=" => vectorize_dyad { |inst, x, y|
            x >= y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is greater than or equal to <code>y</code>, <code>false</code> otherwise. See also: <a href="#>="><code>&gt;=</code></a>.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "≥" => vectorize_dyad { |inst, x, y|
            x >= y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is less than or equal <code>y</code>, <code>false</code> otherwise.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "<=" => vectorize_dyad { |inst, x, y|
            x <= y
        },
        #<<
        # Returns <code>true</code> if <code>x</code> is less than or equal <code>y</code>, <code>false</code> otherwise. See also: <a href="#<="><code>&lt;=</code></a>.
        # @type x (*)
        # @type y (*)
        # @return bool
        # @genre operator/logic
        #>>
        "≤" => vectorize_dyad { |inst, x, y|
            x <= y
        },
        #<<
        # Returns a range from <code>x</code> up to <code>y</code>. If <code>x</code> and <code>y</code> are both functions, returns a function <code>f[...args]</code> which calls <code>x[...Map[y, args]]</code>.
        # @type x number|fn
        # @type y number|fn
        # @return [number]|fn
        # @genre operator
        # @example Print[3:6]
        # @example ?? [3, 4, 5, 6]
        # @example add_rev := Add:Reverse
        # @example Print[add_rev[23, 45]]
        # @example ?? 86
        #>>
        ":" => vectorize_dyad { |inst, x, y|
            if AtState.func_like?(x) && AtState.func_like?(y)
                lambda { |inst, *args|
                    x[inst, *args.map { |e|
                        y[inst, e]
                    }]
                }
            else
                (x..y).to_a
            end
        },
        #<<
        # Returns a range from <code>x</code> up to <code>y</code>. If <code>x &gt; y</code>, returns a reversed range.
        # @type x number
        # @type y number
        # @return [number]
        # @genre operator
        # @example Print[3::6]
        # @example ?? [3, 4, 5, 6]
        # @example Print[6::3]
        # @example ?? [6, 5, 4, 3]
        # @example Print[-3::-6]
        # @example ?? [-3, -4, -5, -6]
        #>>
        "::" => vectorize_dyad { |inst, x, y|
            if x < y
                @@operators[":"][inst, x, y]
            else
                @@operators[":"][inst, y, x].reverse
            end
        },
        #<<
        # Returns a range from <code>x</code> to <code>y</code>, inclusive.
        # @type x number
        # @type y number
        # @return [number]
        # @genre operator
        #>>
        ".." => vectorize_dyad { |inst, x, y|
            (x..y).to_a
        },
        #<<
        # Returns a range from <code>x</code> to <code>y</code>, inclusive. See also: <a href="#.."><code>..</code></a>.
        # @type x number
        # @type y number
        # @return [number]
        # @genre operator
        #>>
        "‥" => vectorize_dyad { |inst, x, y|
            @@operators[".."][inst, x, y]
        },
        #<<
        # Returns a range from <code>x</code> to <code>y</code>, excluding <code>y</code>.
        # @type x number
        # @type y number
        # @return [number]
        # @genre operator
        #>>
        "..." => vectorize_dyad { |inst, x, y|
            (x...y).to_a
        },
        #<<
        # Returns a range from <code>x</code> to <code>y</code>, excluding <code>y</code>. See also: <a href="#..."><code>...</code></a>.
        # @type x number
        # @type y number
        # @return [number]
        # @genre operator
        #>>
        "…" => vectorize_dyad { |inst, x, y|
            @@operators["..."][inst, x, y]
        },
        #<<
        # Returns <code>true</code> if <code>y</code> contains <code>x</code>, otherwise <code>false</code>. See also: <code><a href="#Has">Has</a></code>.
        # @return bool
        # @type x (*)
        # @type y [(*)]
        # @genre operator/logic
        # @example Print[3 in 1:5]
        # @example ?? true
        # @example Print[30 in 1:5]
        # @example ?? false
        #>>
        "in" => lambda { |inst, x, y|
            @@functions["Has"][inst, y, x]
        },
        #<<
        # Returns <code>false</code> if <code>y</code> contains <code>x</code>, otherwise <code>true</code>. See also: <code><a href="#Has">Has</a></code>.
        # @return bool
        # @type x (*)
        # @type y [(*)]
        # @genre operator/logic
        # @example Print[3 !in 1:5]
        # @example ?? false
        # @example Print[30 !in 1:5]
        # @example ?? true
        #>>
        "!in" => lambda { |inst, x, y|
            !@@functions["Has"][inst, y, x]
        },
        #<<
        # Returns <code>true</code> if <code>el</code> is an instance of <code>klass</code>, otherwise <code>false</code>.
        # @return bool
        # @type el (*)
        # @type klass class
        # @genre operator/logic
        #>>
        "is_a" => lambda { |inst, el, klass|
            # https://www.strawpoll.me/15544904/r
            # el.parent == klass rescue false
            el.kind_of? klass rescue false
        },
        #<<
        # Returns <code>true</code> if <code>el</code> is an instance of <code>klass</code>, otherwise <code>false</code>.
        # @return bool
        # @type el (*)
        # @type klass class
        # @genre operator/logic
        #>>
        "is_an" => lambda { |inst, el, klass|
            @@operators["is_a"][inst, el, klass]
        },

        #<<
        # Returns <code>a</code> if <code>a</code> is truthy, <code>b</code> otherwise. Short-circuits.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator/logic
        #>>
        "or" => lambda { |inst, a, b|
            lres = inst.evaluate_node_safe a
            if AtState.truthy? lres
                lres
            else
                inst.evaluate_node_safe b
            end
        },
        #<<
        # Returns <code>a</code> if <code>a</code> is truthy, <code>b</code> otherwise. Short-circuits. See also: <a href="#or"><code>or</code></a>.
        # @type a (*)
        # @type b (*)
        # @return (*)
        # @genre operator/logic
        #>>
        "∨" => held(true, true) { |inst, a, b|
            @@operators["or"][inst, a, b]
        },
        #<<
        # Returns <code>true</code> if exactly one of <code>a</code> and <code>b</code> is truthy, <code>false</code> otherwise.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "xor" => lambda { |inst, a, b|
            # no short circuiting, since both values must be compared
            AtState.truthy?(a) ^ AtState.truthy?(b)
        },
        #<<
        # Returns <code>true</code> if exactly one of <code>a</code> and <code>b</code> is truthy, <code>false</code> otherwise. See also: <a href="#xor"><code>xor</code></a>.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "⊻" => lambda { |inst, a, b|
            @@operators["xor"][inst, a, b]
        },
        #<<
        # Returns <code>true</code> if one of <code>a</code> and <code>b</code> are falsey, <code>false</code> otherwise. Short-circuits.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "nand" => lambda { |inst, a, b|
            if AtState.falsey? inst.evaluate_node_safe a
                true
            elsif AtState.falsey? inst.evaluate_node_safe b
                true
            else
                false
            end
        },
        #<<
        # Returns <code>true</code> if one of <code>a</code> and <code>b</code> are falsey, <code>false</code> otherwise. Short-circuits. See also: <a href="#nand"><code>nand</code></a>.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "⊼" => held(true, true) { |inst, a, b|
            @@operators["nand"][inst, a, b]
        },
        #<<
        # Returns <code>true</code> if both of <code>a</code> and <code>b</code> are truthy, <code>false</code> otherwise. Short-circuits.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "and" => lambda { |inst, a, b|
            lres = inst.evaluate_node_safe a
            if AtState.falsey? lres
                lres
            else
                inst.evaluate_node_safe b
            end
        },
        #<<
        # Returns <code>true</code> if both of <code>a</code> and <code>b</code> are truthy, <code>false</code> otherwise. Short-circuits. See also: <a href="#and"><code>and</code></a>.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "∧" => held(true, true) { |inst, a, b|
            @@operators["and"][inst, a, b]
        },
        #<<
        # Returns <code>a</code> if <code>a</code> is truthy, <code>b</code> otherwise. Short-circuits.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "else" => lambda { |inst, a, b|
            lres = inst.evaluate_node_safe a
            if AtState.truthy? lres
                lres
            else
                inst.evaluate_node_safe b
            end
        },
        #<<
        # Returns <code>true</code> if both of <code>a</code> and <code>b</code> are falsey, <code>false</code> otherwise. Short-circuits.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "nor" => lambda { |inst, a, b|
            if AtState.truthy? inst.evaluate_node_safe a
                false
            elsif AtState.truthy? inst.evaluate_node_safe b
                false
            else
                true
            end
        },
        #<<
        # Returns <code>true</code> if both of <code>a</code> and <code>b</code> are falsey, <code>false</code> otherwise. Short-circuits. See also: <a href="#nor"><code>nor</code></a>.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        #>>
        "⊽" => held(true, true) { |inst, a, b|
            @@operators["nor"][inst, a, b]
        },
        #<<
        # Returns <code>false</code> if <code>b</code> is truthy, <code>a</code> otherwise.
        # @type a (*)
        # @type b (*)
        # @return bool
        # @genre operator/logic
        # @example even_nonpos := { Even[_] not Positive[_] }
        # @example Print[Select[even_nonpos, [-4, -3, -2, 0, 2, 3, 4]]]
        # @example ?? [-4, -2, 0]
        #>>
        "not" => lambda { |inst, a, b|
            # A && !B
            rres = inst.evaluate_node_safe b
            if AtState.truthy? rres
                false
            else
                inst.evaluate_node_safe a
            end
        },
        #<<
        # Returns the intersection of <code>a</code> and <code>b</code>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre operator
        #>>
        "∩" => lambda { |inst, a, b|
            a & b
        },
        #<<
        # Returns the union of <code>a</code> and <code>b</code>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre operator
        #>>
        "∪" => lambda { |inst, a, b|
            a | b
        },
        #<<
        # Returns the symmetric difference between <code>a</code> and <code>b</code>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre operator
        #>>
        "∆" => lambda { |inst, a, b|
            (a | b) - (a & b)
        },
        #<<
        # Returns the <code>a</code> without all elements of <code>b</code>. See also: <a href="#Complement"><code>Complement</code></a>.
        # @type a [(*)]
        # @type b [(*)]
        # @return [(*)]
        # @genre operator
        #>>
        "Ø" => lambda { |inst, a, b|
            a - b
        },
        #<<
        # Removes all <code>b</code> from <code>a</code>. See also: <a href="#Remove"><code>Remove</code></a>.
        # If <code>a</code> is a fucntion, then folds <code>a</code> over <code>b</code>/
        # @type a [(*)]|fn
        # @type b (*)
        # @return [(*)]
        # @example Print[1:5 ^^ 2]
        # @example ?? [1, 3, 4, 5]
        # @example Print[[1, 2, 2, 3, 2, 4, 5, 3, 2, 1, 3] ^^ 2]
        # @example ?? [1, 3, 4, 5, 3, 1, 3]
        # @example Print[Add ^^ 1:5]
        # @example ?? 15
        # @genre operator
        #>>
        "^^" => lambda { |inst, a, b|
            if AtState.func_like? a
                @@functions["Fold"][inst, a, b]
            else
                @@functions["Remove"][inst, a, b]
            end
        },
        #<<
        # Removes all <code>b</code> from <code>a</code>. See also: <a href="#^^"><code>a ^^ b</code></a>.
        # @type a [(*)]
        # @type b (*)
        # @return [(*)]
        # @genre operator
        #>>
        "⩓" => lambda { |inst, a, b|
            @@functions["Remove"][inst, a, b]
        },


        ## -- functional -- #
        "@" => lambda { |inst, f, g|
            if AtState.func_like? g
                lambda { |inst, *args|
                    f[inst, g[inst, *args]]
                }
            else
                f[g] rescue f[inst, g]
            end
        },
        "@@" => lambda { |inst, f, g|
            if AtState.func_like? g
                lambda { |inst, *args| f[inst, *g[inst, *args]] }
            else
                f[inst, *g] rescue @@functions["FlatGet"][inst, f, g]
            end
        },
        "@%" => lambda { |inst, f, g|
            lambda { |inst, *args|
                g[inst, *args]
                f[inst]
            }
        },
        "##" => lambda { |inst, f, g|
            @@operators["@"][inst, f, g]
        },
        "#" => lambda { |inst, x, y|
            Train.new *x, *y
        },
        "'" => @@functions["Tie"],
        "''" => @@functions["TieArray"],
        "&" => lambda { |inst, a, b|
            if AtState.func_like? a
                lambda { |inst, *args|
                    a[inst, *args, b]
                }
            elsif AtState.func_like? b
                lambda { |inst, *args|
                    b[inst, a, *args]
                }
            else
                if Array === a
                    b = inst.enlist(b)
                    key = @@operators["%"][inst, @@functions["Integers"][inst, a], b.size]
                    @@functions["Get"][inst, b, key]
                else
                    resize(inst.enlist(b), a)
                end
            end
        },
        "&:" => lambda { |inst, a, b|
            if AtState.func_like? a
                lambda { |inst, *args| a[inst, b, *args] }
            elsif AtState.func_like? b
                lambda { |inst, *args| b[inst, *args, a] }
            else
                raise AttacheUnimplementedError.new("&: is not defined for non-functional arguments", inst.position)
            end
        },
        "=>" => @@functions["Map"],
        "&>" => lambda { |inst, f, l|
            f = @@unary_operators["&"][inst, f]
            @@functions["Map"][inst, f, l]
        },
        "@>" => lambda { |inst, f, l|
            f = @@unary_operators["@"][inst, f]
            f[inst, l]
        },
        "⇒" => @@functions["Map"],
        ":>" => lambda { |inst, source, func|
            source.map { |x| func[inst, x] }
        },
        "↠" => lambda { |inst, source, func|
            @@operators[":>"][inst, source, func]
        },

        "<:" => lambda { |inst, arr, get|
            # Basically a <: b  <===>  ZipWith[Get, a, b]
            if Array === get
                transposed = true
                inds = begin
                    get.transpose
                rescue
                    transposed = false
                    get
                end
                res = arr.zip(inds).map { |e, i|
                    @@functions["Get"][inst, e, i]
                }
                if transposed
                    res.transpose
                else
                    res
                end
            else
                arr.map { |e| e[get] }
            end
        },
        "↞" => lambda { |inst, arr, get|
            @@operators["<:"][inst, arr, get]
        },
        "\\" => @@functions["Select"],
        "~" => lambda { |inst, left, right|
            @@functions["Count"][inst, left, right]
        },
        #<<
        # Returns a ConfigureValue key-value pair of <code>key</code> and <code>value</code>. Used for configurable functions and hashes.
        # @type key expr
        # @type value (*)
        # @return ConfigureValue
        # @genre operator
        #>>
        "->" => lambda { |inst, key, value|
            if key.is_a?(Node) && key.head.raw == "V"
                # make a function
                params = key.children.map(&:raw)
                value.params = params
                value
            elsif key.is_a?(Token) && key.type == :word
                ConfigureValue.new key.raw, value, raw: true
            elsif key.is_a?(Node)
                keyval = inst.evaluate_node key
                ConfigureValue.new keyval, value, raw: false
            else
                keyval = inst.get_value key
                ConfigureValue.new keyval, value, raw: false
            end
        },
        #<<
        # Returns a ConfigureValue key-value pair of <code>key</code> and <code>value</code>. See also: <a href="#->"><code>-&gt;</code></a>.
        # @type key expr
        # @type value (*)
        # @return ConfigureValue
        # @genre operator
        #>>
        "→" => lambda { |inst, key, value|
            @@operators["->"][inst, key, value]
        },
        ";;" => lambda { |inst, x, y| y },
        "!" => lambda { |inst, a, b|
            if AtState.func_like? a
                a[inst, b]
            else
                raise AttacheUnimplementedError.new("a!b is not defined for non-functional arguments", inst.position)
            end
        },
    }

    @@unary_operators = {
        "-" => vectorize_monad { |inst, n| -n },
        "#" => lambda { |inst, n|
            if n.is_a? Train
                lambda { |inst, args|
                    n[inst, *args]
                }
            else
                @@functions["Size"][inst, n]
            end
        },
        # make constant
        "`" => lambda { |inst, n|
            lambda { |inst, *discard|
                n
            }
        },
        #<<
        # Forces <code>func</code> to use parent scope.
        # @type func fn
        # @return fn
        # @operator
        # @genre unary operator
        # @example a .= 5
        # @example Call[{ a .= 3 }]
        # @example Print[a]
        # @example ?? 5
        # @example Call['{ a .= 93 }]
        # @example Print[a]
        # @example ?? 93
        #>>
        "'" => lambda { |inst, func|
            func.descend = func.ascend = false
            func
        },
        # matrix size
        "##" => lambda { |inst, n|
            dim n
        },
        "±" => vectorize_monad { |inst, a|
            [a, @@unary_operators["-"][inst, a]]
        },
        "/" => lambda { |inst, r|
            if r.is_a? String
                make_regex r
            elsif AtState.func_like? r
                lambda { |inst, *args, last|
                    r[inst, last]
                }
            else
                raise AttacheUnimplementedError.new("/ is not defined for `#{r.class.name}`", inst.position)
            end
        },
        "\\" => lambda { |inst, f|
            if AtState.func_like? f
                lambda { |inst, first, *args|
                    f[inst, last]
                }
            else
                raise AttacheUnimplementedError.new("\\ is not defined for `#{f.class.name}`", inst.position)
            end
        },
        # vectorize
        "@" => lambda { |inst, f|
            if AtState.func_like? f
                vectorize { |inst, *args|
                    f[inst, *args]
                }
            else
                inst.get_value f
            end
        },
        # equiv. f[*x]
        "&" => lambda { |inst, f|
            if AtState.func_like? f
                lambda { |inst, *args|
                    # p args
                    # (lambda{|*b|p b})[*args]
                    f[inst, *args.flatten(1)]
                }
            else
                f.to_s
            end
        },
        # reverses arguments
        "~" => vectorize_monad { |inst, f|
            if AtState.func_like? f
                lambda { |inst, *args|
                    f[inst, *args.reverse]
                }
            else
                ~f
            end
        },
        "!" => vectorize_monad { |inst, n|
            if AtState.func_like? n
                lambda { |inst, *args|
                    @@functions["Permutations"][inst, *args].map { |perm|
                        n[inst, perm]
                    }
                }
            else
                factorial n
            end
        },
        "?" => vectorize_monad { |inst, n|
            AtState.truthy? n
        },
        #<<
        # Returns the parent class of <code>el</code>, or <code>nil</code> if it doesn't exist.
        # @type el (*)
        # @return class|nil
        # @genre unary operator
        #>>
        "parentof" => lambda { |inst, el|
            el.parent rescue nil
        },
        #<<
        # Returns the type of <code>el</code>.
        # @type el (*)
        # @return Type
        # @genre unary operator
        #>>
        "typeof" => lambda { |inst, el|
            AtState.typeof(el)
        },
        #<<
        # Returns <code>false</code> if <code>b</code> is truthy, <code>true</code> otherwise.
        # @type arg (*)
        # @return bool
        # @genre unary operator
        #>>
        "not" => lambda { |inst, arg|
            AtState.falsey? inst.evaluate_node arg
        },
        "..." => lambda { |inst, arg|
            Applicator.new arg
        },
        "…" => lambda { |inst, arg|
            @@unary_operators["..."][inst, arg]
        },
    }
end
