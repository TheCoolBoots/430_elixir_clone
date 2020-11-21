defmodule IdC do
  defstruct [:num]
end

defmodule AppC do
	defstruct [:body, :args]
end

defmodule LamC do
  defstruct [:body, :params]
end

defmodule NumV do
	@enforce_keys [:val]
	defstruct [:val]
end

defmodule StrV do
	@enforce_keys [:val]
	defstruct [:val]
end

defmodule BoolV do
	@enforce_keys [:val]
	defstruct [:val]
end

defmodule PrimV do
	@enforce_keys [:func]
	defstruct [:func]
end

defmodule CloV do
	@enforce_keys [:body, :params, :clo_env]
	defstruct [:body, :params, :clo_env]
end

defmodule M do

	anonFunc = fn () -> IO.puts "passed function" end

	def interp(expr, _env) do
		cond do
			(expr.__struct__ == NumV) -> expr
			(expr.__struct__ == StrV) -> expr
			(expr.__struct__ == BoolV) -> expr
			(expr.__struct__ == PrimV) -> 1
			(expr.__struct__ == CloV)-> 1
			(expr.__struct__ == IdC) -> 1
			(expr.__struct__ == AppC) -> 1
			(expr.__struct__ == CondC) -> 1
			(expr.__struct__ == LamC) -> 1
			true -> "ERROR"
		end
	end

	def test(func) do
		func.()
	end

	def main() do

		test1 = %StrV{val: "magic"}
		IO.puts(test1.val)
		test(anonFunc)
	end

end

M.main()

# newCHANGE
