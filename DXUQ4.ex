defmodule NumC do
  defstruct [:num]
end

defmodule IdC do
  defstruct [:sym]
end

defmodule StrC do
  defstruct [:str]
end

defmodule LamC do
  defstruct [:args, :body]
end

defmodule AppC do
  defstruct [:func, :args]
end

defmodule NumV do
  defstruct [:n]
end

defmodule StrV do
  defstruct [:str]
end

defmodule BoolV do
  defstruct [:bool]
end

defmodule PrimV do
  defstruct [:op]
end

defmodule ClosV do
  defstruct [:args, :body, :env]
end

defmodule Main do
  def main do
    test = %StrC{str: "Hello"}
    IO.puts(test.str)
  end
end

Main.main
