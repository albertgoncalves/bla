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

    def test_fib_lazy(self):
        self.into_test("fib_lazy", 0, "1836311903\n", "")

    def test_fib_loop(self):
        self.into_test("fib_loop", 0, "1836311903\n", "")

    def test_fib_rec(self):
        self.into_test("fib_rec", 0, "55\n", "")

    def test_function_pointers(self):
        self.into_test("function_pointers", 0, "-990\n", "")

    def test_heap(self):
        self.into_test("heap", 0, "-2\n3\n7\n", "")

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

    def test_type_error(self):
        self.into_test(
            "type_error",
            1,
            "",
            "  ex/type_error.bla:12:16 [ type error ]\n",
        )


if __name__ == "__main__":
    main()
