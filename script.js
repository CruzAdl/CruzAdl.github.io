// script.js

// Get references to your stylesheets
const styledStylesheet = document.querySelector('link[href="styles.css"]');
const rawStylesheet = document.createElement('link');
rawStylesheet.href = 'raw-styles.css';
rawStylesheet.rel = 'stylesheet';

// Get a reference to the button
const styleSwitchButton = document.getElementById('style-switch-button');

// Track the current style state
let styled = true;

// Function to toggle styles
function toggleStyles() {
    if (styled) {
        // Switch to raw styles
        styledStylesheet.disabled = true;
        rawStylesheet.disabled = false;
        styled = false;
    } else {
        // Switch to styled styles
        styledStylesheet.disabled = false;
        rawStylesheet.disabled = true;
        styled = true;
    }
}

// Add click event listener to the button
styleSwitchButton.addEventListener('click', toggleStyles);
