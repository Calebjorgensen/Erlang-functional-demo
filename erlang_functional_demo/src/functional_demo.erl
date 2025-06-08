%%% functional_demo.erl

-module(functional_demo).
-export([main/0, factorial/1, fibonacci/1, list_length/1, is_palindrome/1, process_example/0]).

%%% Main function to demonstrate all the features
main() ->
    io:format("Factorial of 5: ~p~n", [factorial(5)]),
    io:format("Fibonacci of 6: ~p~n", [fibonacci(6)]),
    io:format("Length of list [1,2,3,4]: ~p~n", [list_length([1,2,3,4])]),
    io:format("Is [1,2,3,2,1] a palindrome? ~p~n", [is_palindrome([1,2,3,2,1])]),
    io:format("Map *2 on [1,2,3]: ~p~n", [lists:map(fun(X) -> X * 2 end, [1,2,3])]),
    io:format("Filter >2 from [1,2,3,4]: ~p~n", [lists:filter(fun(X) -> X > 2 end, [1,2,3,4])]),
    process_example(),
    ok.

%%% Factorial using recursion and guards
factorial(N) when N == 0 -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

%%% Fibonacci using recursion and pattern matching
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) -> fibonacci(N - 1) + fibonacci(N - 2).

%%% List length using recursion
list_length([]) -> 0;
list_length([_ | T]) -> 1 + list_length(T).

%%% Check if a list is a palindrome
is_palindrome(L) -> L == lists:reverse(L).

%%% Process example - Stretch Challenge
process_example() ->
    Pid = spawn(fun receiver/0),
    Pid ! {self(), "hello"},
    receive
        Response -> io:format("Received from process: ~p~n", [Response])
    after 1000 ->
        io:format("No response received~n")
    end.

%%% Receiver process
receiver() ->
    receive
        {From, Msg} ->
            io:format("Receiver got: ~p~n", [Msg]),
            From ! {reply, "Message received"}
    end.
