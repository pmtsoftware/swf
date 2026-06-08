if (window.login) {
    window.login.addEventListener('submit', async (e) => {
        e.preventDefault();
        const emailField = document.querySelector("#login input[name='email']");
        if (emailField) {
            const accountName = emailField.value;
            const accountDisplayName = emailField.value;

            const response = await fetch(`/webauthn/login/begin`, {
                method: "POST",
                headers: { 'Content-Type': 'application/json' },
                // body: JSON.stringify({
                //     accountName: accountName,
                // }),
                body: JSON.stringify(accountName),
                credentials: "include"
            });
            if (!response.ok) {
                console.error(await response.text())
                return
            }
            const beginResponse = await response.json();
            console.log('Response: ', beginResponse);
            const publicKey = PublicKeyCredential.parseRequestOptionsFromJSON(beginResponse)
            console.log('Public key: ', publicKey);
            const credential = (await navigator.credentials.get({publicKey}));
            console.log('Credential: ', credential);

            const completeResponse = await fetch(`/webauthn/login/complete`, {
                method: "POST",
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(credential),
                credentials: "include"
            });
            if (!completeResponse.ok) {
                console.error(await completeResponse.text())
                return
            }

            alert('Yeahhhh!!!! You are authenticated!');
        }
    })
} else {
    alert('Button NOT found!');
}
