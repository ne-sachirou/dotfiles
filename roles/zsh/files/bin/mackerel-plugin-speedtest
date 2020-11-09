#!/usr/bin/env ruby

require 'json'

# Metric for Mackerel.
class Metric
  def self.from_s(expression)
    name, value_s, time_s = expression.split("\t")
    new(name, value_s.to_f, Time.at(time_s.to_i))
  end

  attr_reader :name, :value, :time

  def initialize(name, value, time)
    @name = name
    @value = value
    @time = time
  end

  def to_s
    "#{@name}\t#{@value}\t#{@time.to_i}"
  end
end

# Runner for speedtest-cli.
class Speedtest
  def measure(time)
    out_r, out_w = IO.pipe
    err_r, err_w = IO.pipe
    pid = spawn('/usr/local/bin/speedtest --json --secure', out: out_w, err: err_w)
    Process.wait(pid)
    out_w.close
    err_w.close
    raise err_r.read unless $?.success?

    result = JSON.parse(out_r.read)
    [
      Metric.new('speedtest.speed.download', result['download'] / 8, time),
      Metric.new('speedtest.speed.upload', result['upload'] / 8, time),
      Metric.new('speedtest.ping.ping', result['ping'], time)
    ]
  end
end

# Runner for speedtest-cli. Cache the result.
class SpeedtestWithCache
  CACHE_FILE = '/tmp/mackerel-plugin-speedtest'.freeze

  def measure(time)
    metrics = Speedtest.new.measure(time)
    IO.write(CACHE_FILE, metrics.map(&:to_s).join("\n"), mode: 'w', encoding: Encoding::UTF_8)
    metrics
  end

  def prev_metrics
    return [] unless File.exist?(CACHE_FILE)

    IO.readlines(CACHE_FILE, mode: 'r', encoding: Encoding::UTF_8, chomp: true)
      .map { |line| Metric.from_s(line) }
  end
end

def print_meta
  puts '# mackerel-agent-plugin'
  puts({
    graphs: {
      'speedtest.speed': {
        label: 'Speedtest',
        unit: 'bytes',
        metrics: [
          { name: 'download', lebel: 'download' },
          { name: 'upload', lebel: 'upload' }
        ]
      },
      'speedtest.ping': {
        label: 'Speedtest Ping',
        unit: 'float',
        metrics: [
          { name: 'ping', lebel: 'ping' } # seconds
        ]
      }
    }
  }.to_json)
end

# Should run speedtest-cli once in a 10 minutes.
def should_measure?(current_time, prev_metrics)
  duration = 10
  prev_metrics.empty? ||
    prev_metrics[0].time + duration * 60 < current_time ||
    (
      prev_metrics[0].time + 1 * 60 > current_time &&
      (current_time.to_i / 60 % duration).zero?
    )
end

if ENV['MACKEREL_AGENT_PLUGIN_META'] == '1'
  print_meta
  exit 0
end
speedtest = SpeedtestWithCache.new
prev_metrics = speedtest.prev_metrics
time = Time.now
metrics =
  if should_measure?(time, speedtest.prev_metrics)
    speedtest.measure(time)
  else
    prev_metrics.map { |metric| Metric.new(metric.name, metric.value, time) }
  end
metrics.each { |metric| puts metric }
