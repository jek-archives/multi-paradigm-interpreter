import http.server
import socketserver
import json
import subprocess
import os
import sys

PORT = 8080
ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(ROOT_DIR)

class IDEServer(http.server.SimpleHTTPRequestHandler):
    def do_GET(self):
        if self.path == '/':
            self.path = '/gui/index.html'
        return http.server.SimpleHTTPRequestHandler.do_GET(self)

    def do_POST(self):
        if self.path == '/run':
            content_length = int(self.headers['Content-Length'])
            post_data = self.rfile.read(content_length)
            data = json.loads(post_data.decode('utf-8'))
            
            language = data.get('language')
            code = data.get('code')
            
            output = self.run_interpreter(language, code)
            
            self.send_response(200)
            self.send_header('Content-type', 'application/json')
            self.end_headers()
            self.wfile.write(json.dumps({'output': output}).encode('utf-8'))

    def run_interpreter(self, language, code):
        cmd = []
        cwd = PROJECT_ROOT
        
        # Determine command based on language
        if language == 'c':
            cmd = ['./interpreter_c/interpreter_c']
        elif language == 'python':
            # Use the same python executable running this script
            cmd = [sys.executable, 'interpreter_python/ide.py']
        elif language == 'prolog':
            cmd = ['swipl', '-s', 'interpreter_prolog/interpreter.pl', '-g', 'repl', '-t', 'halt']
        elif language == 'haskell':
            cmd = ['./interpreter_haskell/Interpreter']
            
        try:
            # We need to simulate the "exit" command for REPLs that expect it
            input_text = code
            if not input_text.endswith('exit'):
                input_text += '\nexit'

            result = subprocess.run(
                cmd,
                input=input_text,
                cwd=cwd,
                capture_output=True,
                text=True,
                timeout=5
            )
            
            if result.returncode != 0:
                return f"Error (Exit Code {result.returncode}):\n{result.stderr}\n{result.stdout}"
                
            return result.stdout
            
        except FileNotFoundError:
            return f"Error: Interpreter for {language} not found/built.\nDid you run 'make'?"
        except subprocess.TimeoutExpired:
             return "Error: Execution Timed Out (Infite Loop?)"
        except Exception as e:
            return f"System Error: {str(e)}"

# Set directory to project root so we can access interpreter folders
os.chdir(PROJECT_ROOT)

print(f"Starting Multi-Paradigm IDE Server at http://localhost:{PORT}")
print(f"Serving files from {PROJECT_ROOT}")

# Use ThreadingTCPServer if available for potential concurrent requests, 
# otherwise TCPServer is fine for single user
with socketserver.TCPServer(("", PORT), IDEServer) as httpd:
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
