#IO.puts "Hello, world!"
#defmodule Product do
#  defstruct val: "Hello"
#end
#defmodule Main do
##  def main() do
#   test = %Product{}
#    IO.puts(test.val)
#  end
#end

defmodule City do
  defstruct name: "San Francisco", state: "CA"
end

defmodule IdC do
  defstruct [:num]
end

defmodule StrC do
  defstruct [:str]
end

defmodule LamC do
  defstruct [:args, :body]
end

defmodule Main do
  def main do
    test = %City{}
    IO.puts(test.name)
  end
end

Main.main
