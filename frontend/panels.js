
const panels = {
    NONE:          'none',
    LOGIN:         'login',
    CREATE_TICKET: 'create-ticket',
    CREATE_USER:   'create-user'
}

function setVisible(visible) {

    const login =
        document.getElementById('login');

    const createTicket =
        document.getElementById('create-tickets');

    const createUser =
        document.getElementById('create-user');

    switch(visible) {

        case panels.NONE:
            login.classList.remove('visible');
            createTicket.classList.remove('visible');
            createUser.classList.remove('visible');
        break;

        case panels.LOGIN:
            login.classList.add('visible');
            createTicket.classList.remove('visible');
            createUser.classList.remove('visible');
        break;

        case panels.CREATE_TICKET:
            login.classList.remove('visible');
            createTicket.classList.add('visible');
            createUser.classList.remove('visible');
        break;

        case panels.CREATE_USER:
            login.classList.remove('visible');
            createTicket.classList.remove('visible');
            createUser.classList.add('visible');
        break;

        default:
            console.log("Error: setVisible fell through: " + visible);
    }
}

function ticketSelect(event) {

    const ticketId      = event.target.id;
    const ticketElement = document.getElementById(ticketId);
    const columnId      = ticketElement.columnId;

    fetchTicket(columnId, ticketId,
        function(ticket) {
            document.getElementById('input-ticket-name').value = ticket.name;
            document.getElementById('input-ticket-content').value = ticket.content;
            const createTicketSection = document.getElementById('create-tickets');
            createTicketSection.columnId = columnId;
            createTicketSection.ticketId = ticketId;
            createTicketSection.classList.add('visible');
        });
}

function clearLoginPanel() {
    //TODO
}

function clearUserPanel() {
    //TODO
}

function clearTicketPanel() {
    const createTicketSection = document.getElementById('create-tickets');
    createTicketSection.columnId = null;
    createTicketSection.ticketId = null;
    document.getElementById('input-ticket-name').value = '';
    document.getElementById('input-ticket-content').value = '';
}
