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
    return async (event) => {
        const boardId    = getCurrentBoard();
        const strName    = document.getElementById('input-ticket-name').value;
        const strContent = document.getElementById('input-ticket-content').value;
        
        await restApi.createTicket(boardId,
                                   strName,
                                   strContent);
        
        setVisible(panels.NONE);
    };
}

function updateTicket(restApi) {
    return async (event) => {
        const createTicketSection = document.getElementById('create-tickets');
        const columnId   = createTicketSection.columnId;
        const ticketId   = createTicketSection.ticketId;
        const strName    = document.getElementById('input-ticket-name').value;
        const strContent = document.getElementById('input-ticket-content').value;
        
        restApi.updateTicket(columnId,
                             ticketId,
                             strName,
                             strContent);

    };
}

const cancelCreateTicket =
    async (event) => {
        setVisible(panels.NONE);
    };

function deleteTicket(restApi) {
    return async (event) => {
        const createTicketSection = document.getElementById('create-tickets');

        const boardId  = getCurrentBoard();
        const columnId = createTicketSection.columnId;
        const ticketId = createTicketSection.ticketId;

        await restApi.deleteTicket(boardId, columnId, ticketId);
        setVisible(panels.NONE);
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
        // TODO
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
        // TODO
    };

function ticketOnDragStart(event) {

    const ticket = event.target.id;
    const from = document.getElementById(ticket).columnId;

    event.dataTransfer.setData('ticket', ticket);
    event.dataTransfer.setData('from', from);
}