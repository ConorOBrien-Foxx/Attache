# esoteric languages library

# class TapeBase; @@commands = {}; end

def tapeFactory(commands, init=lambda{|inst|}, sup=Object)
    klass = Class.new(sup) {
        def class_var(name)
            self.class.class_variable_get name
        end
        
        def initialize(program)
            @program = program.chars
            @i = 0
            instance_eval &class_var(:@@init)
        end
        
        attr_accessor :program, :i, :field
        
        def jump(location)
            @i = location
        end
        
        def step
            cmd = @program[@i]
            cmds = class_var(:@@commands)
            func = cmds[cmd]
            unless func.nil?
                instance_eval &cmds[cmd]
            end
            @i += 1
        end
        
        def running?
            @program[@i]
        end
        
        def run
            step while running?
        end
    }
    old_cmds = {}
    if sup.class_variables.include? :@@commands
        old_cmds.merge! sup.class_variable_get :@@commands
    end
    old_cmds.merge! commands
    klass.class_variable_set(:@@commands, old_cmds)
    klass.class_variable_set(:@@init, init)
    klass
end

# todo: wrapping
BF = tapeFactory({
    ">" => lambda { |inst|
        @tape[@ptr += 1] ||= 0
    },
    "<" => lambda { |inst|
        @tape[@ptr -= 1] ||= 0
    },
    "+" => lambda { |inst|
        @tape[@ptr] += 1
    },
    "-" => lambda { |inst|
        @tape[@ptr] -= 1
    },
    "." => lambda { |inst|
        putc @tape[@ptr]
    },
    "[" => lambda { |inst|
        if @tape[@ptr].zero?
            jump @loops[@i]
        end
    },
    "]" => lambda { |inst|
        unless @tape[@ptr].zero?
            jump @loops[@i]
        end
    },
}, lambda { |inst|
    @ptr = 0
    @tape = [0]
    # process loops
    positions = []
    @loops = {}
    @program.each_with_index { |chr, i|
        if chr == "["
            positions << i
        elsif chr == "]"
            pos = positions.pop
            @loops[pos] = i
            @loops[i] = pos
        end
    }
})

AtState.function("TapeFactory") { |inst, opts|
    p opts
}

AtState.function("BF") { |inst, opts|
    BF.new(opts).run
}

# WTF = tapeFactory({"q" => lambda{}}, BF)

# inst = BF.new ">>><>."

# inst.run