#!/usr/bin/env ruby
# frozen_string_literal: true

# rubocop:disable Style/GlobalVars

require 'stringio'

$threads = []
$threads_mutex = Mutex.new

def sh(cmd, out: :stdout)
  puts "+#{cmd}"
  out_r, out_w =
    case out
    when :stdout then [nil, $stdout]
    when :stream then IO.pipe
    when :string then StringIO.new.tap { |io| break [io, io] }
    else raise "Unknown out: #{out}"
    end
  th = Thread.new do
    begin
      IO.popen(cmd) do |io|
        io.each_line { |line| out_w << line }
      end
    ensure
      case out
      when :stdout then nil
      when :stream then out_w.close
      when :string then out_w.close_write
      end
      $threads_mutex.synchronize do
        $threads = $threads.reject { |_th| _th == th }
      end
    end
  end
  $threads_mutex.synchronize { $threads << th }
  case out
  when :stdout then th.join
  when :stream then out_r
  when :string
    th.join
    out_r.read
  end
rescue StandardError => e
  out_w.close
  raise e
end

class Version
  def self.from_s(s)
    raise "#{s} is not a version" unless version?(s)

    body, suffix = s.split('-', 2)
    major, minor, patch = body.split('.')
    new(major.to_i, minor.to_i, patch && patch.to_i, suffix)
  end

  def self.version?(s)
    s.match?(/^(?:\d+\.){1,2}\d+(?:-(?!dev|rc).+)?$/i)
  end

  attr_reader :major, :minor, :patch, :suffix, :otp

  def initialize(major, minor, patch, suffix = nil)
    @major = major
    @minor = minor
    @patch = patch
    @suffix = suffix
    m = (@suffix || '').match(/^otp-(\d+)$/)
    @otp = (m[1].to_i if m)
  end

  def to_s
    s = "#{@major}.#{@minor}"
    s += ".#{@patch}" if @patch
    s += "-#{@suffix}" if @suffix
    s
  end

  def ==(rhs)
    return false if rhs.nil?

    @major == rhs.major && @minor == rhs.minor && @patch == rhs.patch && @suffix == rhs.suffix
  end

  def <=>(rhs)
    if @major != rhs.major
      @major <=> rhs.major
    elsif @minor != rhs.minor
      @minor <=> rhs.minor
    elsif @patch != rhs.patch
      (@patch || 0) <=> (rhs.patch || 0)
    elsif @otp && rhs.otp && @otp != rhs.otp
      @otp <=> rhs.otp
    elsif @otp && !rhs.otp
      1
    elsif !@otp && rhs.otp
      -1
    else
      0
    end
  end
end

class ASDF
  def all_versions(plugin)
    sh("asdf list all #{plugin}", out: :stream)
      .each_line
      .map(&:strip)
      .select { |version| Version.version?(version) }
      .map { |version| Version.from_s(version) }
  end

  def check_updates
    plugins.each do |plugin|
      latest = all_versions(plugin).max
      local = versions(plugin).max
      if latest.nil?
        puts "Latest version cannot detected (current is #{local})."
      elsif latest != local
        puts "#{plugin} is upgradable to #{latest} (current is #{local})."
      else
        puts "#{plugin} is latest (current is #{local})."
      end
    end
  end

  def plugins
    sh('asdf plugin list', out: :stream).each_line.map(&:strip)
  end

  def update
    sh 'asdf update'
    sh 'asdf plugin update --all'
  end

  def versions(plugin)
    sh("asdf list #{plugin}", out: :stream)
      .each_line
      .map(&:strip)
      .select { |version| Version.version?(version) }
      .map { |version| Version.from_s(version) }
  end
end

asdf = ASDF.new
# asdf.update
asdf.check_updates
# sh 'asdf plugin list | xargs -t -I{} asdf list {}'
sh %(locate .tool-version | sort | xargs -I{} sh -c 'echo {} && sort {} | awk '"'"'{print"\t"$0}'"'"' && echo')
$threads_mutex.synchronize { $threads.each(&:kill) }

# rubocop:enable Style/GlobalVars
