defmodule IdC do
  defstruct [:id]
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

	def interp(expr, env) do
		cond do
			(expr.__struct__ == NumV) -> expr
			(expr.__struct__ == StrV) -> expr
			(expr.__struct__ == BoolV) -> expr
			(expr.__struct__ == PrimV) -> 1
			(expr.__struct__ == CloV)-> 1
			(expr.__struct__ == IdC) -> lookup_env(env, expr.id)
			(expr.__struct__ == AppC) ->
				interpretedBody = interp(expr.body, env)
				cond do
					length(expr.args) > 2 -> "ERROR: invalid number of arguments"
					true ->
						[firstEl|rest] = expr.args
						second = List.first(rest)
						a = interp(firstEl, env)
						b = interp(second, env)
						IO.puts interpretedBody.(a, b).val
						# interpretedBody.(a, b)
				end
			(expr.__struct__ == CondC) -> 1
			(expr.__struct__ == LamC) -> 1
			true -> "ERROR"
		end
	end

	def init_interp(ast) do
		bindAdd = {:+,  fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val + b.val)}
												true -> "ERROR: invalid operands for +"
											end
										end}
		bindSub = {:-, fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val - b.val)}
												true -> "ERROR: invalid operands for -"
										    end
										end}
		bindMult = {:*, fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val * b.val)}
												true -> "ERROR: invalid operands for *"
											end
										end}
		bindDiv = {:/, fn (a, b) ->
											cond do
												(b.__struct__ == NumV && b.val == 0) -> "ERROR: dividing by 0"
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %NumV{val: (a.val / b.val)}
												true -> "ERROR: invalid operands for /"
											end
										end}
		bindLeq = {:leq, fn (a, b) ->
											cond do
												(a.__struct__ == NumV && b.__struct__ == NumV) -> %BoolV{val: (a.val <= b.val)}
												true -> "ERROR: invalid operands for leq"
											end
										end}
		bindEqual = {:equal?, fn (a, b) ->
											cond do
												((a.__struct__ != PrimV || a.__struct__ != CloV) && (b.__struct__ != PrimV || b.__struct__ != CloV)) ->
													%BoolV{val: (a.val == b.val)}
												true -> %BoolV{val: false}
											end
										end}
		top_env = [bindAdd, bindSub, bindMult, bindDiv, bindLeq, bindEqual]

		# IO.puts lookup_env(top_env, :+)

		interp(ast, top_env)
	end

	def lookup_env(env, target) do
		[first|rest] = env
		cond do
			length(env) == 0 -> "ERROR: symbol not found"
			elem(first, 0) == target -> elem(first, 1)
			true -> lookup_env(rest, target)
		end
	end

	def main() do
		testId = %IdC{id: :+}
		a = %NumV{val: 1}
		b = %NumV{val: 3}
		testAppC = %AppC{body: testId, args: [a, b]}
		init_interp(testAppC)
	end

end


M.main()

# anonFunc = fn () -> IO.puts "passed function" end
# def main() do
# 	test1 = %StrV{val: "magic"}
# 	IO.puts(test1.val)
# 	test(M.anonFunc)
# end
# newCHANGE


# def test(func) do
# 	func.()
# end