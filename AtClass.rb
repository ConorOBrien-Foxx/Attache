#!/usr/bin/ruby

require_relative 'AtState.rb'

class AtClassInstance
    def initialize(inherit, methods, vars)
        @methods = methods
        @vars = vars
    end
    
    attr_accessor :vars, :methods
    
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
                "#{k} = #{v}"
            end
        }.join ", "
        "Class[#{inner}]"
    end
end

# todo: template
class AtClass
    def initialize(inst, body, parent=nil)
        @body = body
        @body.ascend = @body.descend = false
        @inst = inst
    end
    
    def create(*params)
        @inst.locals << {}
        @body[@inst, *params]
        scope = @inst.locals.pop
        scope.delete AtLambda::ARG_CONST
        dhash "scope", scope
        
        methods = {}
        vars = {}
        scope.each { |name, val|
            if AtState.func_like? val
                val.scope = vars
                val.ignore_other = true
                methods[name] = val
            else
                vars[name] = val
            end
        }
        AtClassInstance.new self, methods, vars
    end
end