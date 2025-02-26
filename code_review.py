from antlr4 import FileStream, CommonTokenStream, ParseTreeWalker
from generated.Cobol85Lexer import Cobol85Lexer
from generated.Cobol85Parser import Cobol85Parser
from generated.Cobol85Listener import Cobol85Listener
from antlr4.tree.Tree import TerminalNodeImpl

class CodeReviewListener(Cobol85Listener):
    def __init__(self):
        self.issues = []
        self.current_paragraph = None
        self.paragraph_lengths = {}
        self.nesting_level = 0
        self.max_nesting = {}
    
    def enterParagraph(self, ctx):
        self.current_paragraph = ctx.getText().split()[0]
        self.paragraph_lengths[self.current_paragraph] = 0
        self.max_nesting[self.current_paragraph] = 0
    
    def enterStatement(self, ctx):
        if self.current_paragraph:
            self.paragraph_lengths[self.current_paragraph] += 1

    def enterIfStatement(self, ctx):
        if self.current_paragraph:
            self.nesting_level += 1
            if self.nesting_level > self.max_nesting[self.current_paragraph]:
                self.max_nesting[self.current_paragraph] = self.nesting_level

    def exitIfStatement(self, ctx):
        if self.current_paragraph:
            self.nesting_level -= 1
    
    def analyze_code(self):
        for para, length in self.paragraph_lengths.items():
            if length > 50:
                self.issues.append(f"Long method detected: {para} has {length} statements.")
        
        for para, depth in self.max_nesting.items():
            if depth > 3:
                self.issues.append(f"Deep nesting detected in {para}: nesting level {depth}.")
    
    def get_review_report(self):
        return "\n".join(self.issues) if self.issues else "No significant issues detected."

def perform_code_review(cobol_file_path):
    input_stream = FileStream(cobol_file_path, encoding="utf-8")
    lexer = Cobol85Lexer(input_stream)
    token_stream = CommonTokenStream(lexer)
    parser = Cobol85Parser(token_stream)
    tree = parser.startRule()
    
    listener = CodeReviewListener()
    walker = ParseTreeWalker()
    walker.walk(listener, tree)
    
    listener.analyze_code()
    return listener.get_review_report()

if __name__ == "__main__":
    cobol_code_path = "HACKRANK.cbl"  # Change to your COBOL file path
    report = perform_code_review(cobol_code_path)
    with open("code_review_report.txt", "w", encoding="utf-8") as f:
        f.write(report)
    print("Code review completed. Results saved to code_review_report.txt")
