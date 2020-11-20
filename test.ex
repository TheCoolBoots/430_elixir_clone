defmodule Test do
  defstruct val: "Hello"
end

defmodule Main do
  test = %Test{}
  IO.puts test.val
end
