​@type​ ExprC :: {idC, condC, lamC, appC, Value}

defmodule idC do
  defstruct val: :temp
end

defmodule condC do
  defstruct if: :temp, then: :temp, else: :temp
end

defmodule lamC do
  defstruct body: :temp, ids: []
end

defmodule appC do
  defstruct func:, args: []
end

@type Value :: {numV, strV, boolV, cloV, primV}

defmodule numV do
  defstruct val: 0
end

defmodule strV do
  defstruct val: ""
end

defmodule boolV do
  defstruct val: true
end

defmodule cloV do
  defstruct body: :temp, args: [], clo_env: []
end

defmodule primV do
  defstruct function: :temp
end

defmodule Main do
  test_id = %idC{}
  IO.puts(test_id.val)
end
