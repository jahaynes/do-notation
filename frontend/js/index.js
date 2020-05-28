
function bindings(restApi) {

    // Top bar

    document.getElementById('btn-show-login')
            .addEventListener('click', showLogin);

    document.getElementById('btn-logout')
            .addEventListener('click', logout(restApi));

    document.getElementById('btn-show-create-user')
            .addEventListener('click', showCreateUser);

    document.getElementById('btn-show-create-ticket')
            .addEventListener('click', showCreateTicket);


    // Login panel

    document.getElementById('btn-login')
            .addEventListener('click', login(restApi));

    document.getElementById('btn-cancel-login')
            .addEventListener('click', cancelLogin);


    // Create user panel

    document.getElementById('btn-create-user')
            .addEventListener('click', createUser(restApi));

    document.getElementById('btn-cancel-create-user')
            .addEventListener('click', cancelCreateUser);


    // Create ticket panel

    document.getElementById('btn-create-ticket')
            .addEventListener('click', createTicket(restApi));

    document.getElementById('btn-update-ticket')
            .addEventListener('click', updateTicket(restApi));

    document.getElementById('btn-cancel-create-ticket')
            .addEventListener('click', cancelCreateTicket);

    document.getElementById('btn-delete-ticket')
            .addEventListener('click', deleteTicket);
}

function getBoards(restApi) {
    restApi.withBoards(buildBoards(restApi));
}

function unauthHandler() {
    console.log("Not logged in.");
    setVisible(panels.LOGIN);
}

function main() {
    const restApi = createRestApi(unauthHandler);
    bindings(restApi);
    getBoards(restApi);
}



main();
