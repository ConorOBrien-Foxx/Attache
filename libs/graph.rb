# outputs to svg
require 'nokogiri'

class SVGCanvas
    def initialize(width, height)
        @doc = Nokogiri::HTML("<svg style=\"border: 1px solid\"></svg>")
        @svg = @doc.at_css "svg"

        self['xmlns'] = "http://www.w3.org/2000/svg"
        self['width'] = width
        self['height'] = height
        @bounds = nil
        viewbox 0, 0, width, height
    end

    attr_accessor :svg

    def []=(v, s)
        @svg[v] = s
    end

    def [](v)
        @svg[v]
    end

    [:width, :height].map(&:to_s).each { |prop|
        define_method(prop) { self[prop].to_i }
        define_method(prop + "=") { |v| self[prop] = v.to_s }
    }

    def viewbox(x1, y1, x2, y2)
        @bounds = [x1, y1, x2, y2]
        @svg["viewBox"] = "#{x1} #{y1} #{x2} #{y2}"
    end

    def append(element)
        @svg.add_child element
    end

    def make_element(name, **props)
        res = Nokogiri::XML::Node.new name, @doc
        props.each { |name, value|
            res[name] = value
        }
        res
    end

    def line(x1, y1, x2, y2, stroke="black")
        append make_element("line",
            x1: x1,
            y1: y1,
            x2: x2,
            y2: y2,
            stroke: stroke,
        )
    end

    def rect(x, y, width, height, stroke="black", fill="transparent")
        append make_element("rect",
            x: x,
            y: y,
            width: width,
            height: height,
            stroke: stroke,
            fill: fill,
        )
    end

    def graph_at(x, y, left, right, wrap, step=1, &fn)
        coors = (left..right)
            .step(step)
            .map { |e| [e + x, wrap - fn[e] - y] }
        d = ""
        d += "M " + coors.shift.join(",") + " "
        d += coors.map { |a, b| "L#{a},#{b}" }.join " "
        append "<path d=\"#{d}\" stroke=black fill=transparent />"
    end

    def graph_window(**opts, &fn)
        # sub svg
        width = opts[:width]
        height = opts[:height]
        xmax = opts[:xmax]
        xmin = opts[:xmin]
        ymax = opts[:ymax]
        ymin = opts[:ymin]
        sub = SVGCanvas.new(width, height)
        # transform
        # (0, xmin) to (width, xmax)
        # m = (x - x1) / (y - y1)
        scale = -> x { (xmax - xmin.to_f) / width * x + xmin }
        descale = -> y { (ymax - ymin.to_f) / height * y + ymin }

        coors = (0..width).map { |x_to|
            # encode
            input = scale[x_to]
            val = fn[input]
            y_to = descale[input]
            [x_to, y_to]
        }

        p coors

        d = ""
        d += "M " + coors.shift.join(",") + " "
        d += coors.map { |a, b| "L#{a},#{b}" }.join " "
        sub.append "<path d=\"#{d}\" stroke=black fill=transparent />"

        sub['x'] = opts[:atx]
        sub['y'] = opts[:aty]
        rect opts[:atx], opts[:aty], width, height
        append sub.svg
    end

    def to_s
        @doc.to_s
    end
end

def f(x)
    (x - 4)**2 + 4
end

if $0 == __FILE__
    require 'bigdecimal'
    class BigDecimal; def to_s; to_f.to_s; end; end
    can = SVGCanvas.new(100, 100)
    can.width = 500
    can.height = 500
    can.graph_window(
        xmin: -10,
        xmax: 10,
        ymin: -10,
        ymax: 10,
        step: 1,
        atx: 10,
        aty: 10,
        width: 80,
        height: 80,
    ) { |x| x }
    # puts can.svg
    puts can.to_s
end
