defmodule Test do
  defstruct val: "Hello"
  
  def test() do
	IO.puts "hello world"
	end
end

defmodule Main do
  test = %Test{}
  IO.puts test.val
  IO.puts "hello #{test.val}"
  Test.test()
end
