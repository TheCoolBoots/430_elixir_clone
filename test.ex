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
  defstruct name: "San Framcisco", state: "CA"
end

defmodule Main do
  def main do
    test = %City{}
    IO.puts(test.name)
  end
end

Main.main
