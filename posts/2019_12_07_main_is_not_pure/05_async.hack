public async function f(): Awaitable<int> {
  // killer startup business logic here
  return 5;
}

public async function differentBusinessLogic(): Awaitable<void> {
  f_awaitable = f();
  f_value = await f_awaitable
  return f_value + 100;
}
