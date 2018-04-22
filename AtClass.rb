#!/usr/bin/ruby

require_relative 'AtState.rb'

class AtClassInstance
    def initialize(parent, methods, vars, privates)
        @methods = methods
        @vars = vars
        @privates = privates
        @parent = parent
    end

    attr_accessor :vars, :methods, :parent

    def to_s
        inspect
    end

    def [](prop)
        @vars[prop] || @methods[prop]
    end

    def inspect
        @vars.delete AtLambda::ARG_CONST
        # p @vars.keys
        inner = @vars.map { |k, v|
            if v === self
                "#{k} = <recursive>"
            else
                "#{k} = #{v.inspect}"
            end
        }

        inner.concat @privates.map { |k, v|
            "Private[#{k}]"
        }

        inner = inner.join ", "

        "Class[#{inner}]"
    end
end

# todo: template
class AtClass
    def initialize(inst, body, parent=nil, name: "anon")
        @body = body
        @body.ascend = @body.descend = false
        @inst = inst
        @name = name
    end

    def create(*params)
        @inst.locals << {}
        privates = {}

        @inst.define_local "Private", held(true) { |inst, name|
            privates[name.raw] = true
        }

        @inst.saved = []
        @body[@inst, *params]

        scope = @inst.locals.pop
        scope.delete AtLambda::ARG_CONST

        scope.delete "Private"

        methods = {}
        vars = {}
        all = {}
        scope.each { |name, val|
            if AtState.func_like? val
                if AtLambda === val
                    val.scope = all
                    val.ignore_other = true
                end
                if privates[name]
                    privates[name] = val
                    methods[name] = lambda { |inst, *ignore|
                        AtError.new "Access Error", "Attempting to access private method #{name.inspect}."
                    }
                else
                    methods[name] = val
                end
            elsif privates[name]
                privates[name] = val
            else
                vars[name] = val
            end
        }

        all.merge! vars
        all.merge! methods
        all.merge! privates

        AtClassInstance.new self, methods, vars, privates
    end

    def inspect
        "Class #{@name.inspect}"
    end
end
