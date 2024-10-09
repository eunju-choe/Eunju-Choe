from flask import Flask

app = Flask(__name__)

@app.route('/')
def home():
    return "Welcome to the Home Page!"

@app.route('/about')
def about():
    return "This is the About Page!"

@app.route('/contact')
def contact():
    return "This is the Contact Page!"

if __name__ == '__main__':
    app.run(debug=True)