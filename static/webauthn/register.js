
if (window.register) {
    window.register.addEventListener('submit', async (e) => {
        e.preventDefault();
        const emailField = document.querySelector("#register input[name='email']");
        if (emailField) {
            const accountName = emailField.value;
            const accountDisplayName = emailField.value;
            const response = await fetch(`/webauthn/register/begin`, {
                method: "POST",
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    accountName: accountName,
                    accountDisplayName: accountDisplayName,
                }),
                credentials: "include"
            });

            if (! response.ok) {
                alert(await response.text());
                return
            }
            const options = await response.json();
            console.log("Options:", options);
            try {
                const publicKey = PublicKeyCredential.parseCreationOptionsFromJSON(options); 
                const credential = await navigator.credentials.create({ publicKey });
                console.log("Credential created:", credential.toJSON());

                const completeResponse = await fetch(`/webauthn/register/complete`, {
                    credentials: "include",
                    method: "POST",
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify(credential),
                });
                if (! completeResponse.ok) {
                    alert(await completeResponse.text())
                    return
                }
                const msg = await completeResponse.json();
                console.log("Completed: ", msg);
            } catch (error) {
                console.log("Error:", error); 
            }
        }
    })
} else {
    alert('Button NOT found!');
}
