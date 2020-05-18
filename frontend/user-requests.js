const createUser = 
    async (username, password) => {
        const body = { 'username':    username
                     , 'rawpassword': password
                     };
        const response =
            await fetch('/user', {
                method: 'POST',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        const json = await response.json();            
    }

const login = 
    async (username, password) => {

        clearLoginError();

        const body = { 'username':    username
                     , 'rawpassword': password
                     };
        const response =
            await fetch('/login', {
                method: 'POST',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        const errSection =
            document.getElementById('login-error-section');

        const errMsg =
            document.getElementById('login-error-message');

        switch (response.status) {

            case 200:
                clearLoginError();
                break;

            case 401:
                const errTxt = await response.text();
                setLoginError(errTxt);
        }

        return response;
    }

function setLoginError(errTxt) {
    const errSection = document.getElementById('login-error-section');
    const errMsg = document.getElementById('login-error-message');
    errMsg.innerText = errTxt;
    errSection.classList.add('active');     
}

function clearLoginError() {
    const errSection = document.getElementById('login-error-section');
    const errMsg = document.getElementById('login-error-message');
    errMsg.innerText = '';
    errSection.classList.remove('active');     
}

const logout = 
    async () => {
        const response =
            await fetch('/logout', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        return response;
    }