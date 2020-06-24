
const panels = {
    NONE:          'none',
    LOGIN:         'login',
    CREATE_COLUMN: 'create-column',
    CREATE_TICKET: 'create-ticket',
    SIGNUP:        'signup',
    SHARE_BOARD:   'share-board'
}

function clearAllInputs() {

    // Clear login inputs
    document.getElementById('input-login-user-name').value = '';
    document.getElementById('input-login-user-password').value = '';
    
    // Clear create column inputs
    document.getElementById('input-column-name').value = '';

    // Clear create user inputs
    document.getElementById('input-user-name').value = '';
    document.getElementById('input-user-password').value = '';

    // Clear create ticket inputs
    document.getElementById('input-ticket-name').value = '';
    document.getElementById('input-ticket-content').value = '';
}

function setVisible(visible) {

    clearAllInputs();

    const login =
        document.getElementById('login');

    const createColumn =
        document.getElementById('create-column');

    const createTicket =
        document.getElementById('create-tickets');

    const signup =
        document.getElementById('signup');

    const shareBoard =
        document.getElementById('share-board');

    login.classList.remove('visible');
    createColumn.classList.remove('visible');
    createTicket.classList.remove('visible');
    signup.classList.remove('visible');
    shareBoard.classList.remove('visible');

    switch(visible) {

        case panels.NONE:
        break;

        case panels.LOGIN:
            login.classList.add('visible');
        break;

        case panels.CREATE_COLUMN:
            createColumn.classList.add('visible');
        break;

        case panels.CREATE_TICKET:
            createTicket.classList.add('visible');
        break;

        case panels.SIGNUP:
            signup.classList.add('visible');
        break;

        case panels.SHARE_BOARD:
            shareBoard.classList.add('visible');
        break;

        default:
            console.log("Error: setVisible fell through: " + visible);
    }
}

function ticketSelect(restApi) {

    return async (event) => {

        const ticketId      = event.target.id;
        const ticketElement = document.getElementById(ticketId);
        const columnId      = ticketElement.columnId;

        restApi.withTicket(columnId, ticketId,
            function(ticket) {
                document.getElementById('btn-create-ticket').setAttribute("disabled", true);
                document.getElementById('btn-update-ticket').removeAttribute("disabled");
                document.getElementById('input-ticket-name').value = ticket.name;
                document.getElementById('input-ticket-content').value = ticket.content;
                const createTicketSection = document.getElementById('create-tickets');
                createTicketSection.columnId = columnId;
                createTicketSection.ticketId = ticketId;
                createTicketSection.classList.add('visible');
            });
    };
}

function setCurrentBoard(boardId) {
    document.getElementById('boards').activeBoardId = boardId;
}

function getCurrentBoard() {
    return document.getElementById('boards').activeBoardId;
}
