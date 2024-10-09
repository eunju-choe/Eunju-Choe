from flask import Flask, render_template # type: ignore

app = Flask(__name__)

@app.route('/')
def home():
    return "Welcome to the Home Page!"

@app.route('/about')
def about():
    return render_template('about.html')

@app.route('/contact')
def contact():
    return "This is the Contact Page!"

if __name__ == '__main__':
    app.run(debug=True)