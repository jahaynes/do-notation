function createRestApi(unauthHandler) {

    return { "login"        : restLogin
           , "logout"       : restLogout
           , "createUser"   : restCreateUser
           , "withBoards"   : restWithBoards(unauthHandler)
           , "withBoard"    : restWithBoard(unauthHandler)
           , "createBoard"  : restCreateBoard
           , "deleteBoard"  : restDeleteBoard
           , "withColumns"  : restWithColumns
           , "withTicket"   : restWithTicket
           , "createTicket" : restCreateTicket
           , "updateTicket" : restUpdateTicket
           , "deleteTicket" : restDeleteTicket
           , "moveTicket"   : restMoveTicket
           };
}

const restLogin =

    async (username,
           password,
           clearErrors,
           success,
           showError) => {

        clearErrors();

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

        switch (response.status) {

            case 200:
                success(response);
                break;

            case 401:
                showError(await response.text());
                break;
        }
    }

const restLogout = 
    async () => {
        const response =
            await fetch('/logout', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        switch(response.status) {
            case 200:
                break;
            default:
                console.log("Could not logout!: " + response.status);
        }
    }

const restCreateUser =
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

function restWithBoards(unauthHandler) {
    return async (f) => {
        const response = await fetch('/boards');
        switch (response.status) {

            case 200:
                f(await response.json());
                break;

            case 401:
                unauthHandler();
                break;
        }
    }
}

function restWithBoard(unauthHandler) {
    return async (boardId, f) => {

        const response = await fetch('/board?board=' + boardId);

        switch (response.status) {

            case 200:
                const board = await response.json();
                f(board);
                break;

            case 401:
                unauthHandler();
                break;
        }
    }
}
    
const restCreateBoard = 
    async (boardName) => {
        const response =
            await fetch('/board', {
                method: 'POST',
                body: '"' + boardName + '"',
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        var success = false;
        var boardId = '';

        switch(response.status) {

            case 200:
                boardId = await response.json();
                success = true;
                break;

            case 401:
                //TODO
                console.log("auth error");
                break;
        }

        return { "success": success
               , "boardId": boardId
               };
    }

const restDeleteBoard = 
    async (boardId) => {
        const response =
            await fetch('/board', {
                method: 'DELETE',
                body: '"' + boardId + '"',
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        var success = false;

        switch(response.status) {

            case 200:
                success = true;
                console.log("deleted");
                break;

            case 401:
                console.log("auth error");
                break;

            case 403:
                console.log("Couldn't delete - not empty");
                break;

            case 404:
                console.log("board not found");
                break;
        }

        return success;
    }

// TODO - singular?
const restWithColumns =
    async (columns, f) => {

        for (const columnNo in columns) {

            let err = false;
            const columnId = columns[columnNo].columnid;

            const response = await fetch('/column?columnId=' + columnId);

            switch(response.status) {

                case 200:
                    const column = await response.json();
                    f(columnNo, column);
                    break;

                case 401:
                    err = true;
                    //  authFailHandler(); // TODO
                    break;
            }

            if(err) {
                break;
            }
        }
    }

const restWithTicket =
    async (columnId, ticketId, f) => {
        const response = await fetch('/ticket?columnId=' + columnId + "&ticketId=" + ticketId);
        const json = await response.json();
        f(json);

        //TODO check status
    }

const restCreateTicket = 
    async (boardId, name, content) => {
        const body = { 'boardId': boardId
                     , 'name': name
                     , 'content': content
                     };
        fetch('/ticket', {
            method: 'POST',
            body: JSON.stringify(body),
            headers: {
                'Content-Type': 'application/json',
            }
        });

        //TODO check status
    }

const restUpdateTicket =
    async (columnId, ticketId, name, content) => {
        const body = { 'columnId': columnId
                     , 'ticketId': ticketId
                     , 'name': name
                     , 'content': content
                     };
        const response =
            await fetch('/ticket', {
                method: 'PUT',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json',
                }
            });

        //TODO check status
    }

const restDeleteTicket = 
    async (boardId, columnId, ticketId) => {
        const body = { 'columnId': columnId
                     , 'ticketId': ticketId
                     };
        const response =
            await fetch('/ticket', {
                method: 'DELETE',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        //TODO check status
    }

const restMoveTicket =
    async (boardId, from, to, ticket) => {
        const body = { 'board': boardId
                     , 'from': from
                     , 'to': to
                     , 'ticket': ticket
                    };
        const response =
            await fetch('/ticket/move', {
                method: 'POST',
                body: JSON.stringify(body),
                headers: {
                    'Content-Type': 'application/json'
                }
            });

        //TODO check status
    }
