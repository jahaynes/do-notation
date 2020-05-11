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

        return response;
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