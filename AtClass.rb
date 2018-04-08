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
        "Class[#{@vars.map { |k, v| "#{k} = #{v}" }.join ", "}]"
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
        
        methods = {}
        vars = {}
        scope.each { |name, val|
            if AtState.func_like? val
                val.scope = vars
                methods[name] = val
            else
                vars[name] = val
            end
        }
        AtClassInstance.new self, methods, vars
    end
end