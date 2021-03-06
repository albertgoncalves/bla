#!/usr/bin/env python3

from unittest import main, TestCase
from subprocess import run

__unittest = True


class Tests(TestCase):
    def into_test(self, file, expected_code, expected_stdout, expected_stderr):
        result = run(" && ".join([
            "cd \"$WD\"",
            f"./bin/com ex/{file}.bla build/{file}.blc",
            f"./bin/vm build/{file}.blc",
        ]).encode(), capture_output=True, shell=True)
        self.assertEqual(result.returncode, expected_code)
        self.assertEqual(result.stdout.decode(), expected_stdout)
        self.assertEqual(result.stderr.decode(), expected_stderr)

    def test_ackermann_peter(self):
        self.into_test(
            "ackermann_peter",
            0,
            "5 13 29 61 125 253 509\n",
            "",
        )

    def test_as(self):
        self.into_test("as", 0, "-567\n", "")

    def test_discard(self):
        self.into_test("discard", 0, "789\n", "")

    def test_div(self):
        self.into_test("div", 0, "21\n", "")

    def test_early_return(self):
        self.into_test("early_return", 0, "1\n", "")

    def test_effect(self):
        self.into_test("effect", 0, "-123\n", "")

    def test_empty_block(self):
        self.into_test("empty_block", 0, "1\n", "")

    def test_exit(self):
        self.into_test("exit", 123, "", "")

    def test_fib_lazy(self):
        self.into_test("fib_lazy", 0, "1836311903\n", "")

    def test_fib_loop(self):
        self.into_test("fib_loop", 0, "1134903170\n", "")

    def test_fib_rec(self):
        self.into_test("fib_rec", 0, "55\n", "")

    def test_function_pointers(self):
        self.into_test("function_pointers", 0, "-990\n", "")

    def test_heap(self):
        self.into_test("heap", 0, "-2\n12\n16\n", "")

    def test_hello_world(self):
        self.into_test("hello_world", 0, "Hello, world!\n", "")

    def test_more_nested_calls(self):
        self.into_test("more_nested_calls", 0, "0\n1\n", "")

    def test_nested_calls(self):
        self.into_test("nested_calls", 0, "-234\n", "")

    def test_parse_error(self):
        self.into_test(
            "parse_error",
            1,
            "",
            "  ex/parse_error.bla:14:23 [ parse error ]\n",
        )

    def test_pre_compile_error(self):
        self.into_test(
            "pre_compile_error",
            1,
            "",
            "  ex/pre_compile_error.bla:4:5 [ pre-compile error ]\n",
        )

    def test_spawn(self):
        self.into_test(
            "spawn",
            0,
            "2\n3\n500\n1\n",
            "",
        )

    def test_type_error(self):
        self.into_test(
            "type_error",
            1,
            "",
            "  ex/type_error.bla:12:16 [ type error ]\n",
        )

    def test_unreachable_break(self):
        self.into_test(
            "unreachable_break",
            1,
            "",
            "  ex/unreachable_break.bla:4:9 [ pre-compile error ]\n",
        )

    def test_unreachable_return(self):
        self.into_test(
            "unreachable_return",
            1,
            "",
            "  ex/unreachable_return.bla:7:5 [ pre-compile error ]\n",
        )


if __name__ == "__main__":
    main()
