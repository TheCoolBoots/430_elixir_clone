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

#defmodule City do
#  defstruct name: "San Francisco", state: "CA"
#end

#defmodule IdC do
#  defstruct [:num]
#end

#defmodule StrC do
#  defstruct [:str]
#end

#defmodule LamC do
#  defstruct [:args, :body]
#end

#defmodule Main do
#  def main do
#    test = %City{}
#    IO.puts(test.name)
#  end
#end

defmodule ExprC do
  defmodule NumC do
    defstruct [:num]
  end

  defmodule IdC do
    defstruct [:id]
  end

  defmodule StrC do
    defstruct [:str]
  end

  defmodule IfC do
    defstruct [:test, :then, :else]
  end

  defmodule LamC do
    defstruct [:args, :body]
  end

  defmodule AppC do
    defstruct [:func, :args]
  end

  defmodule IfC do
    defstruct [:test, :then, :else]
  end
end

IO.puts(%IdC{id: :a})

defmodule ParseIt do
  def parse(sexp) do

  end
end

#Main.main
defmodule DoMath do
  def add(x, y), do: x + y
  def subtract(x, y), do: x - y
end

ExUnit.start()
defmodule  MyTest do
  use ExUnit.Case
  use ExUnit.Case, async: true
  import DoMath
  test "the truth" do
    assert 1 + 1 == 2
  end
  test "help" do
    assert add(1, 2) == 3
  end

end
