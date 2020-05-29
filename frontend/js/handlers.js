const showLogin =
    async(event) => {
        setVisible(panels.LOGIN);
    };

function logout(restApi) {
    return async (event) => {
        await restApi.logout();
        location = location;
    };
}

const showCreateUser =
    async (event) => {
        setVisible(panels.CREATE_USER);
    };

const showCreateTicket =
    async (event) => {
        setVisible(panels.CREATE_TICKET);
    };

function createTicket(restApi) {

    const boardSelectByIdImpl = boardSelectById(restApi);

    return async (event) => {
        const boardId    = getCurrentBoard();
        const strName    = document.getElementById('input-ticket-name').value;
        const strContent = document.getElementById('input-ticket-content').value;

        setVisible(panels.NONE);

        // Create ticket & refresh page
        restApi.createTicket(boardId,
                             strName,
                             strContent);
                            
        boardSelectByIdImpl(boardId);
    };
}

function updateTicket(restApi) {

    const boardSelectByIdImpl = boardSelectById(restApi);

    return async (event) => {
        const createTicketSection = document.getElementById('create-tickets');
        const columnId   = createTicketSection.columnId;
        const ticketId   = createTicketSection.ticketId;
        const strName    = document.getElementById('input-ticket-name').value;
        const strContent = document.getElementById('input-ticket-content').value;

        setVisible(panels.NONE);

        // Update ticket & refresh page
        await restApi.updateTicket(columnId,
                                   ticketId,
                                   strName,
                                   strContent);
        boardSelectByIdImpl(getCurrentBoard());
    };
}

const cancelCreateTicket =
    async (event) => {
        setVisible(panels.NONE);
    };

function deleteTicket(restApi) {

    const boardSelectByIdImpl = boardSelectById(restApi);

    return async (event) => {
        const createTicketSection = document.getElementById('create-tickets');

        const boardId  = getCurrentBoard();
        const columnId = createTicketSection.columnId;
        const ticketId = createTicketSection.ticketId;

        setVisible(panels.NONE);

        // Delete ticket & refresh page
        await restApi.deleteTicket(boardId,
                                   columnId,
                                   ticketId);
        boardSelectByIdImpl(boardId);
    };
}

function login(restApi) {
    return async (event) => {

        const strName     = document.getElementById('input-login-user-name').value;
        const strPassword = document.getElementById('input-login-user-password').value;
        const errSection  = document.getElementById('login-error-section');
        const errMsg      = document.getElementById('login-error-message');

        function clearLoginError() {
            errMsg.innerText = '';
            errSection.classList.remove('active');     
        }

        function setLoginError(errTxt) {
            errMsg.innerText = errTxt;
            errSection.classList.add('active');     
        }

        function loginSuccess() {
            setVisible(panels.NONE);
            clearLoginError();
            location = location;
        }

        restApi.login(strName,
                      strPassword,
                      clearLoginError,
                      loginSuccess,
                      setLoginError);
    };
}

const cancelLogin =
    async (event) => {
        setVisible(panels.NONE);
    };

function createUser(restApi) {
    return async (event) => {
        const strName     = document.getElementById('input-user-name').value;
        const strPassword = document.getElementById('input-user-password').value;
        await restApi.createUser(strName,
                                 strPassword);
        setVisible(panels.NONE);
    };
}

const cancelCreateUser =
    async (event) => {
        setVisible(panels.NONE);
    };

function ticketOnDragStart(event) {

    const ticket = event.target.id;
    const from = document.getElementById(ticket).columnId;

    event.dataTransfer.setData('ticket', ticket);
    event.dataTransfer.setData('from', from);
}
