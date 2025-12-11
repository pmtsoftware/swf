
if (window.register) {
    window.register.addEventListener('submit', async (e) => {
        e.preventDefault();
        alert('Button pressed');
    })
} else {
    alert('Button NOT found!');
}
