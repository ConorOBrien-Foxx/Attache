# outputs javascript code
require 'json'

class ChartJSWrapper
    def initialize(type)
        @type = type
        @datasets = []
    end

    def add_dataset(label, data)
        @datasets << {
            label: label,
            data: data,
            backgroundColor: [
                'rgba(255, 99, 132, 0.2)',
                'rgba(54, 162, 235, 0.2)',
                'rgba(255, 206, 86, 0.2)',
                'rgba(75, 192, 192, 0.2)',
                'rgba(153, 102, 255, 0.2)',
                'rgba(255, 159, 64, 0.2)'
            ],
            borderColor: [
                'rgba(255,99,132,1)',
                'rgba(54, 162, 235, 1)',
                'rgba(255, 206, 86, 1)',
                'rgba(75, 192, 192, 1)',
                'rgba(153, 102, 255, 1)',
                'rgba(255, 159, 64, 1)'
            ],
        }
    end

    def render(source="graph")
        <<~END
            new Chart(#{source.inspect}, {
                type: #{@type.inspect},
                data: [
                    #{@datasets.map(&:to_json).join "\n"}
                ],
                options: {},
            });
        END
    end
end

AtState.function("Graph") { |inst, type, data|
    res = ""
    res += '<canvas id="graph"></canvas>' + "\n"
    res += '<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.4.0/Chart.min.js"></script>' + "\n"
    chart = ChartJSWrapper.new(type)
    chart.add_dataset("Data", data)
    res += "<script>\n#{chart.render}\n</script>"
    res
}
