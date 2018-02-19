# esoteric languages library

# class TapeBase; @@commands = {}; end

def tapeFactory(commands, field, sup=Object)
    klass = Class.new(sup) {
        def class_var(name)
            self.class.class_variable_get name
        end
        
        def initialize(program)
            @program = program
            @i = 0
            @field = class_var(:@@field).dup
        end
        
        attr_accessor :program, :i, :field
        
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
    klass.class_variable_set(:@@field, field)
    klass
end

BF = tapeFactory({
    ">" => lambda { |inst|
        @field["ptr"] += 1
    },
    "<" => lambda { |inst|
        @field["ptr"] -= 1
    },
    "." => lambda { |inst|
        puts @field["ptr"]
    }
}, { "ptr" => 0 })

AtState.function("TapeFactory") { |inst, opts|
    p opts
}

# WTF = tapeFactory({"q" => lambda{}}, BF)

# inst = BF.new ">>><>."

# inst.run