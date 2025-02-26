from antlr4 import FileStream, CommonTokenStream, ParseTreeWalker
from antlr4.tree.Tree import TerminalNodeImpl
from generated.Cobol85Lexer import Cobol85Lexer
from generated.Cobol85Parser import Cobol85Parser
from generated.Cobol85Listener import Cobol85Listener
from antlr4 import InputStream

class ASTListener(Cobol85Parser):
    def enterEveryRule(self, ctx):
        print(ctx.getText())

def generate_ast(cobol_code_path):
    with open(cobol_code_path, "r", encoding="utf-8") as f:
        cobol_code = f.read()
    input_stream = InputStream(cobol_code)
    lexer = Cobol85Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Cobol85Parser(token_stream)
    tree = parser.startRule()  # Change according to the COBOL grammar entry point
    return tree

def print_ast(tree, output_file, indent=0):
    with open(output_file, "w", encoding="utf-8") as f:
        def write_node(node, indent_level):
            if isinstance(node, TerminalNodeImpl):
                f.write("  " * indent_level + node.getText() + "\n")
            else:
                f.write("  " * indent_level + node.__class__.__name__ + "\n")
                for i in range(node.getChildCount()):
                    write_node(node.getChild(i), indent_level + 1)
        write_node(tree, indent)

if __name__ == "__main__":
    cobol_code_path = "Car_Rental.cbl"  # Path to COBOL source file
    output_file_path = "ast_output.txt"  # Path to save AST output
    ast_root = generate_ast(cobol_code_path)
    print_ast(ast_root, output_file_path)