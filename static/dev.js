try {
    const ws = new WebSocket("ws://127.0.0.1:3069")
    console.log("Connected to local dev server")

    ws.onopen = () => {
        console.log('Connection is open')
    }

    ws.onmessage = (e) => {
        console.log("Got message")
        console.log(e.data)

        switch (e.data) {
            case "build":
                setTimeout(() => {
                    if (window.location) {
                        window.location.reload()
                    }
                }, 500)
                break
            default:
                const words = e.data.split(' ')
                const el = document.querySelector(`link[href^='${words[1]}']`)
                if (el) {
                    console.log(el)
                    const css = document.createElement('link')
                    css.href = `${words[1]}?foo=${new Date().getTime()}`
                    css.rel = "stylesheet"
                    el.replaceWith(css)
                }
                break
        }
    }

    setInterval(() => {
        console.log(`WebSocket state: ${ws.readyState}`)
    }, 1000)
} catch (error) {
    console.log(error)
}
