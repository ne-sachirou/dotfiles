#!/usr/bin/env ruby
# frozen_string_literal: true

# rubocop:disable Style/Documentation, Style/GlobalVars

require 'stringio'

$threads = []
$threads_mutex = Mutex.new

class StdoutOut
  attr_reader :reader, :writer

  def initialize
    @reader = nil
    @writer = $stdout
  end

  def close_writer
    nil
  end

  def read
    nil
  end
end

class StreamOut
  attr_reader :reader, :writer

  def initialize
    @reader, @writer = IO.pipe
  end

  def close_writer
    @writer.close
  end

  def read
    nil
  end
end

class StringOut
  attr_reader :reader, :writer

  def initialize
    io = StringIO.new
    @reader = io
    @writer = io
  end

  def close_writer
    @writer.close_write
  end

  def read
    @reader.read
  end
end

def sh(cmd, out: :stdout)
  puts "+#{cmd}"
  out_rw = { stdout: StdoutOut, stream: StreamOut, string: StringOut }[out].new
  thread = Thread.new do
    IO.popen(cmd) do |io|
      io.each_line { |line| out_rw.writer << line }
    end
  ensure
    out_rw.close_writer
    $threads_mutex.synchronize do
      $threads = $threads.reject { |th| th == thread }
    end
  end
  $threads_mutex.synchronize { $threads << thread }
  return out_rw.reader if out == :stream

  thread.join
  out_rw.read
rescue StandardError => e
  out_rw.close_writer
  raise e
end

class Version
  def self.from_s(str)
    raise "#{str} is not a version" unless version?(str)

    body, suffix = str.split('-', 2)
    major, minor, patch = body.split('.')
    new(major.to_i, minor.to_i, patch && patch.to_i, suffix)
  end

  def self.version?(str)
    str.match?(/^(?:\d+\.){1,2}\d+(?:-(?!dev|rc).+)?$/i)
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
    str = "#{@major}.#{@minor}"
    str += ".#{@patch}" if @patch
    str += "-#{@suffix}" if @suffix
    str
  end

  def ==(other)
    return false if other.nil?

    @major == other.major && @minor == other.minor && @patch == other.patch && @suffix == other.suffix
  end

  def <=>(other)
    if @major != other.major
      @major <=> other.major
    elsif @minor != other.minor
      @minor <=> other.minor
    elsif @patch != other.patch
      (@patch || 0) <=> (other.patch || 0)
    elsif @otp && other.otp && @otp != other.otp
      @otp <=> other.otp
    elsif @otp && !other.otp
      1
    elsif !@otp && other.otp
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
asdf.update
asdf.check_updates
# sh 'asdf plugin list | xargs -t -I{} asdf list {}'
sh %(locate .tool-version | sort | xargs -I{} sh -c 'echo {} && sort {} | awk '"'"'{print"\t"$0}'"'"' && echo')
$threads_mutex.synchronize { $threads.each(&:kill) }

# rubocop:enable Style/Documentation, Style/GlobalVars
