
// alert("Hej jestem tu")

// if (window.location) {
//     window.location.reload()
// }
//

try {
    const ws = new WebSocket("ws://127.0.0.1:3069")
    console.log("Connected to local dev server")

    ws.onopen = () => {
        console.log('Connection is open')
    }

    ws.onmessage = (e) => {
        console.log("Got message")
        console.log(e.data)
    }

    setInterval(() => {
        console.log(`WebSocket state: ${ws.readyState}`)
    }, 1000)
} catch (error) {
    console.log(error)
}
